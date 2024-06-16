package sdql
package backend

import munit.Assertions.munitPrint
import sdql.analysis.TypeInference
import sdql.ir.ExternalFunctions._
import sdql.ir._

import scala.PartialFunction.{cond, condOpt}

object CppCodegen {
  private type TypesCtx = TypeInference.Ctx
  private type CallsCtx = List[CallCtx]
  private sealed trait CallCtx
  private case class LetCtx(name: String) extends CallCtx
  private case class SumCtx(k:String, v: String, isLoad: Boolean) extends CallCtx
  private type LoadsCtx = Set[Sym]

  private val reDate = "^(\\d{4})(\\d{2})(\\d{2})$".r
  private val resultName = "result"

  def apply(e: Exp): String = {
    val (csvBody, loadsCtx) = CsvBodyWithLoadsCtx(e)
    val mainBody = run(e)(Map(), List(), loadsCtx)
    // slightly wasteful to redo type inference - but spares us having to return the type at every recursive call
    val tpe = TypeInference(e)
    s"""|#include "../runtime/headers.h"
        |
        |const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
        |const auto SEPARATOR = rapidcsv::SeparatorParams('|');
        |$csvBody
        |
        |int main() {
        |$mainBody
        |${cppPrintResult(tpe)}
        |}
        |""".stripMargin
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    if (checkNoLetBindings(e)) {
      return run(LetBinding(Sym(resultName), e, DictNode(Nil)))
    }

    e match {
      case LetBinding(x @ Sym(name), e1, e2) =>
        val cpp_e1 = e1 match {
          // codegen for loads was handled in a separate tree traversal
          case Load(_, DictType(RecordType(_), IntType)) => ""
          case _ => s"auto $name = ${run(e1)(typesCtx, List(LetCtx(name)) ++ callsCtx, loadsCtx)};"
        }
        val cpp_e2 = e2 match {
          case DictNode(Nil) => ""
          case _ => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), callsCtx, loadsCtx)
        }
        cpp_e1 + cpp_e2

      case Sum(k, v, e1, e2) =>
        generateSum(k, v, e1, e2)

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
          case _ => s" else {\n${ifElseBody(e2)}\n}"
        }
        s"if (${run(cond)}) {${ifElseBody(e1)}\n}$elseBody"

      case Cmp(Get(e1, e2), DictNode(Nil), "!=")
        if cond(TypeInference.run(e1)) { case DictType(kt, _) => TypeInference.run(e2) == kt } =>
          s"${run(e1)}.contains(${run(e2)})"
      case Cmp(DictNode(Nil), Get(e1, e2), "!=")
        if cond(TypeInference.run(e1)) { case DictType(kt, _) => TypeInference.run(e2) == kt } =>
        s"${run(e1)}.contains(${run(e2)})"
      case Cmp(e1, e2, cmp) =>
        s"${run(e1)} $cmp ${run(e2)}"

      case FieldNode(e1, f) =>
        TypeInference.run(e1) match {
          case tpe: RecordType =>
            val idx = (tpe.indexOf(f): @unchecked) match { case Some(idx) => idx }
            e1 match {
              case Sym(name)
                if callsCtx.exists(x => cond(x) { case SumCtx(k, v, true) => name == k || name == v})
                => s"$name.$f[i]"
              case _ => s" /* $f */ std::get<$idx>(${run(e1)})"
            }
          case tpe => raise(
            s"expected ${RecordType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
          )
        }

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
        callsCtx.flatMap(x => condOpt(x) { case ctx: SumCtx => ctx }).headOption match {
          case Some(SumCtx(k, v, true)) if name == k || name == v => s"$name[i]"
          case _ => name
        }

      case DictNode(Nil) =>
        ""
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

      case Concat(v1 @ RecNode(fs1), v2 @ RecNode(fs2)) => run(
        {
          val (fs1m, fs2m) = fs1.toMap -> fs2.toMap
          val common =
            fs1.filter(x1 => fs2m.contains(x1._1)).map(x1 => (x1._1, x1._2, fs2m(x1._1)))
          if(common.isEmpty)
            RecNode(fs1 ++ fs2)
          else
            if(common.forall(x => x._2 == x._3))
              RecNode(fs1 ++ fs2.filter(x2 => !fs1m.contains(x2._1)))
            else
              raise(s"`concat($v1, $v2)` with different values for the same field name")
        }
      )
      case Concat(Sym(name1), Sym(name2)) =>
        s"std::tuple_cat($name1, $name2)"
      case Concat(e1, e2) =>
        val _ = TypeInference.run(e)
        raise(
          s"${Concat.getClass.getSimpleName} currently requires both arguments " +
            s"${RecNode.getClass.getSimpleName.init} or ${Sym.getClass.getSimpleName.init}, " +
            s"not ${e1.simpleName} and ${e2.simpleName}"
        )

      case _ => raise(
        f"""Unhandled ${e.simpleName} in
           |${munitPrint(e)}""".stripMargin
      )
    }
  }

  private def generateSum(k: Sym, v: Sym, e1: Exp, e2: Exp)
                         (implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
    var (tpe, typesLocal) = TypeInference.sum_infer_type_and_ctx(k, v, e1, e2)
    typesLocal ++= Map(Sym(agg) -> tpe)

    val isLoad = cond(e1) { case e1Sym: Sym => loadsCtx.contains(e1Sym) }
    val callsLocal = List(SumCtx(k=k.name, v=v.name, isLoad=isLoad)) ++ callsCtx

    val sumBody = e2 match {
      case _: LetBinding | _: IfThenElse =>
        run(e2)(typesLocal, callsLocal, loadsCtx)
      case _ =>
        ifElseBody(e2)(typesLocal, callsLocal, loadsCtx)
    }

    if (isLoad) {
      val e1Name = (e1: @unchecked) match { case Sym(e1Name) => e1Name }
      s"""${cppType(tpe)} (${cppInit(tpe)});
         |for (int i = 0; i < ${e1Name.capitalize}::size(); i++) {
         |const auto &${k.name} = $e1Name;
         |constexpr auto ${v.name} = ${e1Name.capitalize}Values();
         |$sumBody
         |}
         |
         |""".stripMargin
    } else {
      s"""${cppType(tpe)} (${cppInit(tpe)});
         |for (const auto &[${k.name}, ${v.name}] : ${run(e1)}) {
         |$sumBody
         |}
         |""".stripMargin
    }
  }

  private def ifElseBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    assert(checkInSum())
    val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
    e match {
      case _: Sum =>
        raise("nested sum not supported yet")
      case DictNode(seq) =>
        // TODO sdqlpy does emplace aggregating over unique keys
        //  kv => s"$agg.emplace(${run(kv._1)}, ${run(kv._2)});"
        seq.map(kv => s"$agg[${run(kv._1)}] += ${run(kv._2)};").mkString("\n")
      case RecNode(values) =>
        values.map(_._2).zipWithIndex.map({ case (exp, i) => s"get<$i>($agg) += ${run(exp)};" }).mkString("\n")
      case _ =>
        s"$agg += ${run(e)};"
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
        .toSet

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
      pathNameAttrs.map(_._2).map(Sym.apply),
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

  private def flattenExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: RangeNode | _: Load =>
          Iterator()
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

  private def checkNoLetBindings(e: Exp)(implicit callsCtx: CallsCtx) =
    !cond(e) { case _: LetBinding => true } && !callsCtx.exists(cond(_) { case _: LetCtx  => true })

  private def checkInSum()(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case _: SumCtx  => true })

  private def cppPrintResult(tpe: Type) = tpe match {
    case _: DictType =>
      s"""for (const auto &[key, val] : $resultName) {
         |std::cout << key << ":" << val << std::endl;
         |}""".stripMargin
    case RecordType(attrs) =>
      val body =
        attrs.map(_.name).zipWithIndex.map(
          { case (name, i) => s""""$name = " << std::get<$i>($resultName)""" }
        ).mkString(""" << ", " << """)
      s"""std::stringstream ss;
         |ss << $body;
         |std::cout << "<" << ss.str() << ">" << std::endl;
         |""".stripMargin
    case _ if tpe.isScalar =>
      s"std::cout << $resultName << std::endl;"
  }
}
