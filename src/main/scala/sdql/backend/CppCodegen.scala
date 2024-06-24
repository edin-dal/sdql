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
  private case class LetCtx(name: String) extends CallCtx
  private case class SumCtx(k:String, v: String, isLoad: Boolean, hint: CodegenHint) extends CallCtx
  private case class SumEnd() extends CallCtx
  private case class IsTernary() extends CallCtx
  private type LoadsCtx = Set[Sym]

  private val vecSize = 6000001;
  private val reDate = "^(\\d{4})(\\d{2})(\\d{2})$".r
  private val resultName = "result"

  private val header = """#include "../runtime/headers.h""""
  private val consts =
    s"""const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
       |const auto SEPARATOR = rapidcsv::SeparatorParams('|');
       |""".stripMargin

  // overload to run multiple queries that share same datasets)
  def apply(expsWithNames: Seq[(Exp, String)]): String = {
    val csvBody = CsvBodyWithLoadsCtx(expsWithNames.map(_._1))._1
    val benchHeader =
      """#include <chrono>
        |using namespace std::chrono;
        |""".stripMargin
    val preamble = s"""$header\n$benchHeader\n\n$consts\n$csvBody\n"""
    val main = expsWithNames
      .map( x =>
        s"""std::cout << "\\n${x._2}" << std::endl;
           |${x._2}();
           |""".stripMargin)
      .mkString(s"int main() {\n", "", "\n}")
    expsWithNames.map( x => this.apply(x._1, Some(x._2), isBenchmark = true)).mkString(preamble, "\n\n", s"\n\n$main")
  }

  def apply(e: Exp, batchedName: Option[String] = None, isBenchmark: Boolean = false): String = {
    val (csvBody, loadsCtx) = CsvBodyWithLoadsCtx(Seq(e))
    val queryBody = run(e)(Map(), List(), loadsCtx)
    val benchStart = if(!isBenchmark) "" else "auto start = high_resolution_clock::now();\n"
    val benchStop = if(!isBenchmark) "" else
      """auto stop = high_resolution_clock::now();
        |auto duration = duration_cast<milliseconds>(stop - start);
        |cout << "Runtime (ms): " << duration.count() << endl;
        |""".stripMargin
    // slightly wasteful to redo type inference - but spares us having to return the type at every recursive run call
    val tpe = TypeInference(e)
    val queryBodyWithPrint =
      s"""$benchStart
         |$queryBody
         |$benchStop
         |${cppPrintResult(tpe)}
         |""".stripMargin
    batchedName match {
      case None => s"""$header\n\n$consts\n$csvBody\n""" ++ s"int main() { $queryBodyWithPrint }"
      case Some(name) => s"void $name() { $queryBodyWithPrint }"
    }
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    if (checkNoLetBindings(e)) {
      return run(LetBinding(Sym(resultName), e, DictNode(Nil)))
    }

    if (checkIsSumBody(e)) {
      val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
      val hint = callsCtx.flatMap(x => condOpt(x) { case SumCtx(_, _, _, hint) => hint }).head
      val callsLocal = List(SumEnd()) ++ callsCtx
      return e match {
        case _: Sum =>
          raise("nested sum not supported yet")
        case DictNode(seq) => seq.map(
            kv => {
              val lhs = run(kv._1)(typesCtx, callsLocal, loadsCtx)
              val rhs = run(kv._2)(typesCtx, callsLocal, loadsCtx)
              hint match {
                case _: NoHint => s"$agg[$lhs] += $rhs;"
                case _: UniqueHint => s"$agg.emplace($lhs, $rhs);"
                case _: VectorHint =>
                  typesCtx(Sym(agg)) match {
                    case DictType(IntType, DictType(IntType, _)) =>
                      e match {
                        case DictNode(Seq((f1: FieldNode, DictNode(Seq((f2: FieldNode, _)))))) =>
                          val lhs = run(f1)(typesCtx, callsLocal, loadsCtx)
                          val rhs = run(f2)(typesCtx, callsLocal, loadsCtx)
                          s"$agg[$lhs].emplace_back($rhs);"
                      }
                    case DictType(IntType, _) =>
                      s"$agg[$lhs] = $rhs;"
                    case tpe =>
                      raise(s"expected nested ${DictType.getClass.getSimpleName.init} " +
                        s"with ${IntType.getClass.getSimpleName.init} keys up to 2-levels deep, " +
                        s"not ${tpe.simpleName}")
                  }
              }
            }
          ).mkString("\n")
        case RecNode(values) =>
          values.map(_._2).zipWithIndex.map(
            { case (exp, i) => s"get<$i>($agg) += ${record(exp)(typesCtx, callsLocal, loadsCtx)};" }
          ).mkString("\n")
        case _ =>
          s"$agg += ${run(e)(typesCtx, callsLocal, loadsCtx)};"
      }
    }

    e match {
      case LetBinding(x @ Sym(name), e1, e2) =>
        val isTernary = !cond(e1) { case _: Sum => true }
        val localCalls = if (isTernary) List(IsTernary()) ++ callsCtx else callsCtx
        val e1Cpp = e1 match {
          case Load(_, DictType(RecordType(_), IntType)) =>
            // codegen for loads was handled in a separate tree traversal
            ""
          case External(Limit.SYMBOL, _) =>
            s"${run(e1)(typesCtx, List(LetCtx(name)) ++ localCalls, loadsCtx)}\n"
          // TODO add a const case for Const assignments
          case _ =>
            s"auto $name = ${run(e1)(typesCtx, List(LetCtx(name)) ++ localCalls, loadsCtx)};"
        }
        val e2Cpp = e2 match {
          case DictNode(Nil) => ""
          case _ => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), callsCtx, loadsCtx)
        }
        e1Cpp + e2Cpp

      case Sum(k, v, e1, e2, hint) =>
        val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
        var (tpe, typesLocal) = TypeInference.sum_infer_type_and_ctx(k, v, e1, e2)
        typesLocal ++= Map(Sym(agg) -> tpe)

        val isLoad = cond(e1) { case e1Sym: Sym => loadsCtx.contains(e1Sym) }
        val callsLocal = List(SumCtx(k=k.name, v=v.name, isLoad=isLoad, hint)) ++ callsCtx

        val body = run(e2)(typesLocal, callsLocal, loadsCtx)

        val init = hint match {
          case _: VectorHint =>
            tpe match {
              case DictType(IntType, DictType(IntType, vt)) =>
                s"vector<vector<${cppType(vt)}>>($vecSize)"
              case DictType(IntType, vt) =>
                s"vector<${cppType(vt)}>($vecSize)"
              case tpe =>
                raise(s"expected nested ${DictType.getClass.getSimpleName.init} " +
                  s"with ${IntType.getClass.getSimpleName.init} keys up to 2-levels deep, " +
                  s"not ${tpe.simpleName}")
            }
          case _ =>
            s"${cppType(tpe)} (${cppInit(tpe)})"
        }

        if (isLoad) {
          val e1Name = (e1: @unchecked) match { case Sym(e1Name) => e1Name }
          s"""$init;
             |for (int i = 0; i < ${e1Name.capitalize}::size(); i++) {
             |const auto &${k.name} = $e1Name;
             |constexpr auto ${v.name} = ${e1Name.capitalize}Values();
             |$body
             |}
             |
             |""".stripMargin
        } else {
          val head = run(e1)(typesLocal, List(SumEnd()) ++ callsLocal, loadsCtx)
          s"""$init;
             |for (const auto &[${k.name}, ${v.name}] : $head) {
             |$body
             |}
             |""".stripMargin
        }

      // not case
      case IfThenElse(a, Const(false), Const(true)) =>
        s"!(${run(a)})"
      // and case
      case IfThenElse(a, b, Const(false)) =>
        s"(${run(a)} && ${run(b)})"
      // or case
      case IfThenElse(a, Const(true), b) =>
        s"(${run(a)} || ${run(b)})"

      case IfThenElse(cond, e1, e2) if checkIsTernary =>
        val callsLocal = List(SumEnd()) ++ callsCtx
        val condBody = run(cond)(typesCtx, callsLocal, loadsCtx)
        val ifBody = run(e1)(typesCtx, callsLocal, loadsCtx)
        val elseBody = run(e2)(typesCtx, callsLocal, loadsCtx)
          s"($condBody) ? $ifBody : $elseBody"

      case IfThenElse(cond, e1, e2) =>
        val condBody = run(cond)(typesCtx, List(SumEnd()) ++ callsCtx, loadsCtx)
        val ifBody = run(e1)
        val elseBody = e2 match {
          case DictNode(Nil) | RecNode(Seq()) | Const(0) | Const(0.0) => ""
          case _ => s" else {\n${run(e2)}\n}"
        }
        s"if ($condBody) {$ifBody\n}$elseBody"

      case Cmp(Get(e1, e2), DictNode(Nil), "!=")
        if cond(TypeInference.run(e1)) { case DictType(kt, _) => TypeInference.run(e2) == kt } =>
        (e1, e2) match {
          // TODO remove this special handling for Q21
          case (Sym("ord_indexed"), FieldNode(Sym("l"), "l_orderkey")) =>
            "ord_indexed[l.l_orderkey[i]] != false"
          case _ =>
            s"${run(e1)}.contains(${run(e2)})"
        }

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
                if callsCtx.exists(x => cond(x) { case SumCtx(k, v, true, _) => name == k || name == v})
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

      case Mult(e1, External(Inv.SYMBOL, args)) =>
        val divisor = args match { case Seq(divisorExp: Exp) => run(divisorExp) }
        s"(${run(e1)} / $divisor)"
      case Mult(e1, e2) =>
        s"(${run(e1)} * ${run(e2)})"

      case Neg(e) =>
        s"-${run(e)}"

      case Const(DateValue(v)) =>
        val yyyymmdd = reDate.findAllIn(v.toString).matchData.next()
        s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
      case Const(v: String) =>
        s""""$v""""
      case Const(v) =>
        v.toString

      case Sym(name) =>
        callsCtx.flatMap(x => condOpt(x) { case ctx: SumCtx => ctx }).headOption match {
          case Some(SumCtx(k, v, true, _)) if name == k || name == v => s"$name[i]"
          case _ => name
        }

      case DictNode(Nil) =>
        ""
      case DictNode(seq) =>
        val localCalls = List(IsTernary()) ++ callsCtx
        seq.map( { case (e1, e2) =>
            val e1Cpp = run(e1)(typesCtx, localCalls, loadsCtx)
            val e2Cpp = run(e2)(typesCtx, localCalls, loadsCtx)
            s"{$e1Cpp, $e2Cpp}"
          })
          .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")

      case RecNode(Seq()) =>
        ""
      case RecNode(values) =>
        val localCalls = List(IsTernary()) ++ callsCtx
        val tpe = TypeInference.run(e) match {
          case tpe: RecordType => tpe
          case tpe => raise(
            s"expression ${e.simpleName} should be of type " +
              s"${RecordType.getClass.getSimpleName.init} not ${tpe.simpleName}"
          )
        }
        values.map(e => run(e._2)(typesCtx, localCalls, loadsCtx)).mkString(s"${cppType(tpe)}(", ", ", ")")

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

      case External(StrStartsWith.SYMBOL, Seq(str, prefix)) =>
        s"${run(str)}.starts_with(${run(prefix)})"
      case External(StrEndsWith.SYMBOL, Seq(str, suffix)) =>
        s"${run(str)}.ends_with(${run(suffix)})"
      case External(SubString.SYMBOL, Seq(str, start, end)) =>
        s"${run(str)}.substr(${run(start)}, ${run(end)})"
      case External(StrIndexOf.SYMBOL, Seq(field: FieldNode, elem, from)) =>
        s"${run(field)}.find(${run(elem)}, ${run(from)})"
      case External(name @ Inv.SYMBOL, _) =>
        raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")
      case External(MaxValue.SYMBOL, Seq(arg)) =>
        assert(!cond(arg) { case sym: Sym => loadsCtx.contains(sym) })
        s"""std::ranges::max_element(${run(arg)}, [](const auto &p1, const auto &p2) {
          |return p1.second < p2.second;
          |})->second;
          |""".stripMargin
      case External(Size.SYMBOL, Seq(arg)) =>
        condOpt(arg) { case sym @ Sym(name) if loadsCtx.contains(sym) => name } match {
          case Some(name) => s"${name.capitalize}::size()"
          case None => s"${run(arg)}.size()"
        }
      case External(Limit.SYMBOL, Seq(sym @ Sym(arg), Const(n: Int), Const(isDescending: Boolean))) =>
        val limit = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
        val tmp = s"tmp_$uuid"
        val (dictTpe, kt, vt) =
          (TypeInference.run(sym): @unchecked) match { case dictTpe @ DictType(kt, vt) => (dictTpe, kt, vt) }
        val recTpe = RecordType(Seq(Attribute("_1", kt), Attribute("_2", vt)))
        val recTpeCpp = cppType(recTpe)
        val cmp = if(isDescending) ">" else "<"
        s"""std::vector<$recTpeCpp> $tmp($n);
           |std::partial_sort_copy(
           |    $arg.begin(), $arg.end(), $tmp.begin(), $tmp.end(),
           |    []($recTpeCpp const &l, $recTpeCpp const &r) { return std::get<1>(l) $cmp std::get<1>(r); });
           |auto $limit = ${cppType(dictTpe)}($tmp.begin(), $tmp.end());
           |""".stripMargin
      case External(name, _)  =>
        raise(s"unhandled function name: $name")

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
      case Concat(e1: Sym, e2: Sym) =>
        s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1: Sym, e2: RecNode) =>
        s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1: RecNode, e2: Sym) =>
        s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1, e2) =>
        val _ = TypeInference.run(e)
        raise(
          s"${Concat.getClass.getSimpleName} requires arguments " +
            s"${RecNode.getClass.getSimpleName.init} and/or ${Sym.getClass.getSimpleName.init}, " +
            s"not ${e1.simpleName} and ${e2.simpleName}"
        )

      case _ => raise(
        f"""Unhandled ${e.simpleName} in
           |${munitPrint(e)}""".stripMargin
      )
    }
  }

  // TODO no longer needed
  private def record(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx) = e match {
    case IfThenElse(cond, e1, e2) => s"(${run(cond)}) ? ${run(e1)} : ${run(e2)}"
    case _ => run(e)
  }

  private def cppInit(tpe: ir.Type): String = tpe match {
    case BoolType => "false"
    case RealType => "0.0"
    case IntType | DateType => "0"
    case _: StringType => "std::string()"
    case _: DictType => "{}"
    case RecordType(attrs) => attrs.map(_.tpe).map(cppInit).mkString(", ")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def cppType(tpe: ir.Type): String = tpe match {
    case BoolType => "bool"
    case RealType => "double"
    case IntType | DateType => "long"
    case _: StringType => "std::string"
    case DictType(key, value) => s"phmap::flat_hash_map<${cppType(key)}, ${cppType(value)}>"
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType).mkString("std::tuple<", ", ", ">")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def CsvBodyWithLoadsCtx(exps: Seq[Exp]): (String, LoadsCtx) = {
    val pathNameAttrs = exps.flatMap( e =>
      iterExps(e)
        .flatMap(
          e => condOpt(e) {
            case LetBinding(Sym(name), Load(path, DictType(RecordType(attrs), IntType)), _) =>
              (path, name, attrs)
          }
        )
    ).toSet

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

  private def iterExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: RangeNode | _: Load =>
          Iterator()
        // 1-ary
        case Neg(e) =>
          iterExps(e)
        case FieldNode(e, _) =>
          iterExps(e)
        case Promote(_, e) =>
          iterExps(e)
        // 2-ary
        case Add(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Mult(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Cmp(e1, e2, _) =>
          iterExps(e1) ++ iterExps(e2)
        case Sum(_, _, e1, e2, _) =>
          iterExps(e1) ++ iterExps(e2)
        case Get(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Concat(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case LetBinding(_, e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        // 3-ary
        case IfThenElse(e1, e2, e3) =>
          iterExps(e1) ++ iterExps(e2) ++ iterExps(e3)
        // n-ary
        case RecNode(values) =>
          values.map(_._2).flatMap(iterExps).toList
        case DictNode(dict) =>
          dict.flatMap(x => iterExps(x._1) ++ iterExps(x._2)).toList
        case External(_, args) =>
          args.flatMap(iterExps).toList
        // unhandled
        case _ => raise(
          f"""Unhandled ${e.simpleName} in
             |${munitPrint(e)}""".stripMargin
        )
      }
    )

  private def checkNoLetBindings(e: Exp)(implicit callsCtx: CallsCtx) =
    !cond(e) { case _: LetBinding => true } && !callsCtx.exists(cond(_) { case _: LetCtx  => true })

  private def checkIsSumBody(e: Exp)(implicit callsCtx: CallsCtx): Boolean = {
    !cond(e) { case _: LetBinding | _: IfThenElse | _: Sum => true } &&  checkActiveSumCtx
  }

  private def checkActiveSumCtx(implicit callsCtx: CallsCtx) = {
    callsCtx.indexWhere(x => cond(x) { case _: SumCtx => true }) match {
      case -1 => false
      case start => callsCtx.indexWhere(x => cond(x) { case _: LetCtx |  _: SumEnd => true }) match {
        case -1 => true
        case end =>
          start < end
      }
    }
  }

  private def checkIsTernary(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case _: IsTernary  => true })

  private def cppPrintResult(tpe: Type) = tpe match {
    // TODO pretty print nested types (e.g. dates inside a tuple which is a dict key)
    case _: DictType =>
      s"""for (const auto &[key, val] : $resultName) {
         |$stdCout << key << ":" << val << std::endl;
         |}""".stripMargin
    case RecordType(attrs) =>
      val body =
        attrs.zipWithIndex.map(
          {
            case (Attribute(name, DateType), i) => s""""$name = " << print_date(std::get<$i>($resultName))"""
            case (Attribute(name, _), i) => s""""$name = " << std::get<$i>($resultName)"""
          }
        ).mkString(""" << ", " << """)
      s"""std::stringstream ss;
         |ss << $body;
         |$stdCout << "<" << ss.str() << ">" << std::endl;
         |""".stripMargin
    case _ if tpe.isScalar =>
      s"$stdCout << $resultName << std::endl;"
  }

  private val stdCout = s"std::cout << std::setprecision (std::numeric_limits<double>::digits10)"

  private def uuid = UUID.randomUUID.toString.replace("-", "_")
}
