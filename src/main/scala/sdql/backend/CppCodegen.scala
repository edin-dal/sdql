package sdql
package backend

import munit.Assertions.munitPrint
import sdql.analysis.TypeInference
import sdql.ir.ExternalFunctions._
import sdql.ir._

import java.util.UUID
import scala.PartialFunction.{cond, condOpt}

object CppCodegen {
  private type TypesCtx = TypeInference.Ctx
  private type CallsCtx = List[CallCtx]
  private sealed trait CallCtx
  private case class LetCtx() extends CallCtx
  private case class LoopCtx(k:String, v: String, agg: String, isSum: Boolean) extends CallCtx
  private type LoadsCtx = Set[Sym]

  private val reDate = "^(\\d{4})(\\d{2})(\\d{2})$".r

  private def uuid = UUID.randomUUID.toString.replace("-", "_")

  def apply(e: Exp): String = {
    val (csvBody, loadsCtx) = CsvBodyWithLoadsCtx(e)
    val mainBody = run(e)(Map(), List(), loadsCtx)
    // slightly wasteful to redo type inference - but spares us having return the type at every recursive call
    val tpe = TypeInference(e)
    val name = "result"
    val printBody = tpe match {
      case _: DictType =>
        s"""for (const auto &[key, val] : $name) {
           |std::cout << key << ":" << val << std::endl;
           |}""".stripMargin
      case _: RecordType =>
        // TODO print out attribute names
        s"std::cout << $name << std::endl;"
      case _ if tpe.isScalar =>
        s"std::cout << $name << std::endl;"
    }
    s"""|#include "../runtime/headers.h"
        |
        |const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
        |const auto SEPARATOR = rapidcsv::SeparatorParams('|');
        |$csvBody
        |
        |int main() {
        |$mainBody
        |$printBody
        |}
        |""".stripMargin
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    if (
      !cond(e) { case _: LetBinding | _: ForLoop | _: Sum => true } &&
      !callsCtx.exists(cond(_) { case _: LetCtx | _: LoopCtx => true })
    ) {
      return s"auto result = ${run(e)(typesCtx, List(LetCtx()) ++ callsCtx, loadsCtx)};"
    }

    e match {
      case LetBinding(x @ Sym(name), e1, e2) =>
        val cpp_e1 = e1 match {
          case Load(_, DictType(RecordType(_), IntType)) =>
            // handled in a previous separate tree traversal
            ""
          case _: ForLoop | _: Sum =>
            val callsLocal = List(LetCtx()) ++ callsCtx
            s"auto $name = ${generateLoop(e1, Some(name))(typesCtx, callsLocal, loadsCtx)}"
          case _ =>
            val callsLocal = List(LetCtx()) ++ callsCtx
            s"auto $name = ${run(e1)(typesCtx, callsLocal, loadsCtx)};"
        }
        val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
        val cpp_e2 = run(e2)(typesLocal, callsCtx, loadsCtx)
        cpp_e1 + cpp_e2

      case _: ForLoop | _: Sum =>
        generateLoop(e)

      // not case
      case IfThenElse(a, Const(false), Const(true)) =>
        s"!(${run(a)})"
      // and case
      case IfThenElse(a, b, Const(false)) =>
        s"(${run(a)} && ${run(b)})"
      // or case
      case IfThenElse(a, Const(true), b) =>
        s"(${run(a)} || ${run(b)})"
      case IfThenElse(cond, e1, e2) =>
        val elseBody = e2 match {
          case DictNode(Nil) => ""
          case _ =>
            s""" else {
               |${ifElseBody(e2)}
               |}""".stripMargin
        }
        s"""if (${run(cond)}) {${ifElseBody(e1)}\n}$elseBody""".stripMargin

      case Cmp(e1, e2: Sym, "∈") =>
        TypeInference.run(e2) match {
          case _: DictType =>
          case tpe => raise(
            s"expression ${e2.simpleName} should be of type " +
              s"${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
          )
        }
        s"${run(e2)}.contains(${run(e1)})"
      case Cmp(e1, e2, cmp) =>
        s"${run(e1)} $cmp ${run(e2)}"

      case e @ FieldNode(Sym(name), f) =>
        // FIXME hack for Q3, Q5, Q9, Q20 (Q1, Q6 work even with fromLoad=true)
        // val fromLoad = name != "k" && name != "v"
        val fromLoad = true
        fieldNode(e, fromLoad)

      case Add(e1, Neg(e2)) =>
        s"(${run(e1)} - ${run(e2)})"
      case Add(e1, e2) =>
        s"(${run(e1)} + ${run(e2)})"

      case Mult(e1, External(name, args)) if name == Inv.SYMBOL =>
        val divisor = args match { case Seq(divisorExp: Exp) => run(divisorExp) }
        s"(${run(e1)} / $divisor)"
      case Mult(e1, e2) =>
        s"(${run(e1)} * ${run(e2)})"

      case Neg(e) =>
        s"-${run(e)}"

      case Const(DateValue(v)) =>
        val yyyymmdd = reDate.findAllIn(v.toString).matchData.next()
        s""""${yyyymmdd.group(1)}-${yyyymmdd.group(2)}-${yyyymmdd.group(3)}""""
      case Const(v: String) =>
        s""""$v""""
      case Const(v) =>
        v.toString

      case Sym(name) =>
        val iter = callsCtx.flatMap(x => condOpt(x) { case LoopCtx(k, v, _, _) => (k, v) }).iterator
        if (iter.hasNext) {
          val(k, v) = iter.next()
          if (name == k || name == v)
            return s"$name[i]"
        }
        name

      case DictNode(Nil) =>
        ""
      // TODO check it's used by Q8 / Q12 (also Q19 to return its result?)
      case DictNode(seq) =>
        seq.map( { case (e1, e2) => s"{${run(e1)}, ${run(e2)}}" })
          .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")

      case RecNode(values) =>
        val tpe = TypeInference.run(e) match {
          case tpe: RecordType => tpe
          case tpe => raise(
            s"expression ${e.simpleName} should be of type " +
              s"${RecordType.getClass.getSimpleName.init} not ${tpe.simpleName}"
          )
        }
        values.map(e => run(e._2)).mkString(s"${cppType(tpe)}(", ", ", ")")

      case Get(e1, e2) => TypeInference.run(e1) match {
        case _: RecordType =>
          s"std::get<${run(e2)}>(${run(e1)})"
        case _: DictType =>
          s"${run(e1)}[${run(e2)}]"
        case tpe => raise(
          s"expected ${RecordType.getClass.getSimpleName.init} or " +
            s"${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
        )
      }

      case External(name, args) => args match {
        case Seq(str, prefix) if name == StrStartsWith.SYMBOL =>
          s"${run(str)}.starts_with(${run(prefix)})"
        case Seq(str, suffix) if name == StrEndsWith.SYMBOL =>
          s"${run(str)}.ends_with(${run(suffix)})"
        case Seq(str, start, end) if name == SubString.SYMBOL =>
          s"${run(str)}.substr(${run(start)}, ${run(end)})"
        case Seq(field: FieldNode, elem, from) if name == StrIndexOf.SYMBOL =>
          s"${run(field)}.find(${run(elem)}, ${run(from)})"
        case _ if name == Inv.SYMBOL =>
          raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")
        case _ =>
          raise(s"unhandled function name: $name")
      }

      case _ => raise(
        f"""Unhandled ${e.simpleName} in
           |${munitPrint(e)}""".stripMargin
      )
    }
  }

  private def generateLoop
  (e: Exp, maybe_agg: Option[String] = None)
  (implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    val (k, v, e1, e2) = TypeInference.unpack_loop(e)
    val e1Sym = e1 match { case e1: Sym => e1 }
    var (tpe, typesLocal) = TypeInference.loop_infer_type_and_ctx(e)

    val agg = maybe_agg match {
      case None => if (callsCtx.exists(cond(_) { case LetCtx() => true })) s"v_$uuid" else "result"
      case Some(agg) => agg
    }
    typesLocal ++= Map(Sym(agg) -> tpe)

    val init = maybe_agg match {
      case None => s"${cppType(tpe)} $agg(${cppInit(tpe)});"
      // when an aggregation variable name is passed
      // we assume the loop is bound to a let statement
      case Some(_) => s"${cppType(tpe)} (${cppInit(tpe)});"
    }

    val isSum = cond(e) { case _: Sum => true }
    val callsLocal = List(LoopCtx(k=k.name, v=v.name, agg=agg, isSum=isSum)) ++ callsCtx
    val loopBody = e2 match {
      case _: LetBinding | _: IfThenElse =>
        run(e2)(typesLocal, callsLocal, loadsCtx)
      case _ =>
        ifElseBody(e2)(typesLocal, callsLocal, loadsCtx)
    }

    // TODO use it to assign Values()
    val _ = loadsCtx.contains(e1Sym)

    s"""$init
        |const auto &${k.name} = ${e1Sym.name};
        |constexpr auto ${v.name} = ${e1Sym.name.capitalize}Values();
        |for (int i = 0; i < ${e1Sym.name.toLowerCase}.size(); i++) {
        |$loopBody
        |}
        |
        |""".stripMargin
  }

  private def ifElseBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    val iter = callsCtx.flatMap(x => condOpt(x) { case LoopCtx(_, _, agg, isSum) => (agg, isSum) }).iterator
    val (agg, isSum) = if (iter.hasNext) iter.next() else raise(
      s"${IfThenElse.getClass.getSimpleName.init}"
        + s" inside ${Sum.getClass.getSimpleName.init}"
        + " needs to know sum context"
    )
     e match {
      case DictNode(Nil) =>
        ""
      case DictNode(Seq((e1, e2))) =>
        assert(cond(typesCtx.get(Sym(agg))) { case Some(_: DictType) => true })
        if (isSum)
          s"$agg[${run(e1)}] += ${run(e2)};"
        else
          s"$agg.emplace(${run(e1)}, ${run(e2)});"
      case RecNode(values) =>
        assert(cond(typesCtx.get(Sym(agg))) { case Some(RecordType(attrs)) => attrs.length == values.length })
        assert(isSum)
        values.map(_._2).zipWithIndex.map({ case (exp, i) => s"get<$i>($agg) += ${run(exp)};" }).mkString("\n")
      case _: ForLoop | _: Sum =>
        raise("nested loop not supported yet")
      case _ =>
        assert(cond(typesCtx.get(Sym(agg))) { case Some(t) => t.isScalar })
        assert(isSum || TypeInference.run(e).isScalar)
        s"$agg += ${run(e)};"
    }
  }

  private def fieldNode(e: FieldNode, fromLoad: Boolean)(implicit typesCtx: TypesCtx) = e match {
    case FieldNode(sym @ Sym(name), f) =>
      TypeInference.run(sym) match {
        case t: RecordType =>
          val idx = t.indexOf(f) match { case Some(idx) => idx}
          if (fromLoad) s"$name.$f[i]" else s" /* $f */ std::get<$idx>($name)"
        case tpe => raise(
          s"expected ${RecordType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
        )
      }
  }

  private def cppInit(tpe: ir.Type): String = tpe match {
    case BoolType => "false"
    case RealType => "0.0"
    case IntType => "0"
    case StringType | DateType => "std::string()"
    case _: DictType => "{}"
    case RecordType(attrs) => attrs.map(_.tpe).map(cppInit).mkString(", ")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def cppType(tpe: ir.Type): String = tpe match {
    case BoolType => "bool"
    case RealType => "double"
    case IntType => "int"
    case StringType | DateType => "std::string"
    case DictType(key, value) => s"phmap::flat_hash_map<${cppType(key)}, ${cppType(value)}>"
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType).mkString("std::tuple<", ", ", ">")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def CsvBodyWithLoadsCtx(e: Exp): (String, LoadsCtx) = {
    val pathNameAttrs =
      flattenExps(e)
        .flatMap(
          e => condOpt(e) {
            case LetBinding(Sym(name), Load(path, DictType(RecordType(attrs), IntType)), _) =>
              (path, name, attrs)
          }
        )
        .distinct

    val csvConsts =
      pathNameAttrs.map({ case (path, name, _) => makeCsvConst(name, path) } ).mkString("", "\n", "\n")
    val structDefs =
      pathNameAttrs.map({ case (_, name, attrs) => makeStructDef(name, attrs) } ).mkString("\n")
    val structInits =
      pathNameAttrs.map({ case (_, name, attrs) => makeStructInit(name, attrs) } ).mkString("\n")
    val valueClasses =
      pathNameAttrs.map({ case (_, name, _) => makeValuesClass(name) } ).mkString("\n")

    (
      List(csvConsts, structDefs, structInits, valueClasses).mkString("\n"),
      pathNameAttrs.map(_._2).map(Sym.apply).toSet,
    )
  }

  private def makeCsvConst(name: String, path: String): String =
    s"""const rapidcsv::Document ${name.toUpperCase}("../$path", NO_HEADERS, SEPARATOR);"""

  private def makeStructDef(name: String, attrs: Seq[Attribute]): String = {
    attrs.map(attr => s"std::vector<${cppType(attr.tpe)}> ${attr.name};")
      .mkString(
        s"struct ${name.capitalize} {\n",
        "\n",
        s"""
           |static unsigned long size() { return ${name.toUpperCase}.GetRowCount(); }
           |};
           |""".stripMargin
      )
  }

  private def makeStructInit(name: String, attrs: Seq[Attribute]): String =
    attrs.zipWithIndex.map(
        {
          case (attr, i) => s"${name.toUpperCase}.GetColumn<${cppType(attr.tpe)}>($i),"
        }
      )
      .mkString(s"const ${name.capitalize} ${name.toLowerCase} {\n", "\n", "\n};\n")

  private def makeValuesClass(name: String): String =
    s"""
       |class ${name.capitalize}Values{
       |public:
       |int operator[](const int i) const { return 0 <= i < ${name.toUpperCase}.GetRowCount(); }
       |};
       |""".stripMargin

  private def flattenExps(e: Exp): List[Exp] =
    List(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: RangeNode | _: Load =>
          List()
        // 1-ary
        case Neg(e) =>
          flattenExps(e)
        case FieldNode(e, _) =>
          flattenExps(e)
        case Promote(_, e) =>
          flattenExps(e)
        // 2-ary
        case Add(e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        case Mult(e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        case Cmp(e1, e2, _) =>
          flattenExps(e1) ++ flattenExps(e2)
        case ForLoop(_, _, e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        case Sum(_, _, e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        case Get(e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        case Concat(e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        case LetBinding(_, e1, e2) =>
          flattenExps(e1) ++ flattenExps(e2)
        // 3-ary
        case IfThenElse(e1, e2, e3) =>
          flattenExps(e1) ++ flattenExps(e2) ++ flattenExps(e3)
        // n-ary
        case RecNode(values) =>
          values.map(_._2).flatMap(flattenExps).toList
        case DictNode(dict) =>
          dict.flatMap(x => flattenExps(x._1) ++ flattenExps(x._2)).toList
        case External(_, args) =>
          args.flatMap(flattenExps).toList
        // unhandled
        case _ => raise(
          f"""Unhandled ${e.simpleName} in
             |${munitPrint(this)}""".stripMargin
        )
      }
      )
}
