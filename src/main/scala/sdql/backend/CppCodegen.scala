package sdql
package backend

import sdql.analysis.TypeInference
import sdql.ir.ExternalFunctions._
import sdql.ir._

import java.util.UUID
import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec

private sealed trait CallCtx
private case class LetCtx(name: String) extends CallCtx
private case object SumStart extends CallCtx
private case object SumEnd extends CallCtx
private case object IsTernary extends CallCtx

private sealed trait Aggregation
private case object SumAgg extends Aggregation
private case object ProdAgg extends Aggregation
private case object MinAgg extends Aggregation
private case object MaxAgg extends Aggregation

object CppCodegen {
  private type TypesCtx = TypeInference.Ctx
  private type CallsCtx = Seq[CallCtx]

  private val vecSize = 6000001
  private val reDate = "^(\\d{4})(\\d{2})(\\d{2})$".r
  private val resultName = "result"
  private val noName = "_"

  private val header = """#include "../runtime/headers.h""""
  private val csvConsts =
    s"""const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
       |const auto SEPARATOR = rapidcsv::SeparatorParams('|');
       |""".stripMargin

  def apply(e: Exp, benchmarkRuns: Int = 0): String = {
    val rewrite = Rewriter(e)
    val csvBody = cppCsvs(Seq(rewrite))
    val queryBody = run(rewrite)(Map(), Seq())
    val benchStart = if (benchmarkRuns == 0) "" else
      s"""HighPrecisionTimer timer;
         |for (${cppType(IntType)} iter = 1; iter <= $benchmarkRuns; iter++) {
         |timer.Reset();
         |""".stripMargin
    val benchStop = if (benchmarkRuns == 0) "" else
      s"""
         |doNotOptimiseAway($resultName);
         |timer.StoreElapsedTime(0);
         |cerr << "*" << " " << flush;
         |if (iter == $benchmarkRuns) {
         |cerr << endl;
         |std::cout << timer.GetMean(0) << " ms" << std::endl;
         |${cppPrintResult(TypeInference(rewrite))}
         |}
         |}""".stripMargin
    s"""$header
       |$csvConsts
       |$csvBody
       |int main() {
       |$benchStart
       |$queryBody
       |$benchStop
       |${if (benchmarkRuns == 0) cppPrintResult(TypeInference(rewrite)) else ""}
       |}""".stripMargin
  }

  @tailrec
  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    if (checkNoLetBindings(e)) { return run(LetBinding(Sym(resultName), e, DictNode(Nil))) }
    if (checkIsSumBody(e)) { return sumBody(e) }

    e match {
      case e: LetBinding => run(e)
      case e: Sum => run(e)
      case e: IfThenElse => run(e)
      case e: Cmp => run(e)
      case e: FieldNode => run(e)
      case e: Add => run(e)
      case e: Mult => run(e)
      case e: Neg => run(e)
      case e: Const => run(e)
      case e: Sym => run(e)
      case e: DictNode => run(e)
      case e: RecNode => run(e)
      case e: Get => run(e)
      case e: External => run(e)
      case e: Concat => run(e)

      // ignore - handled separately in sum
      case Unique(e: Exp) => run(e)
      case Promote(_, e: Exp) => run(e)
      case RangeNode(e: Exp) => run(e)

      case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }
  }

  private def sumBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    cond(e) { case dict @ DictNode(seq, _) if seq.length != 1 => raise(s"unsupported: $dict") }
    val hint = sumHint(e)
    val aggregation = getAggregation(e)
    val callsLocal = Seq(SumEnd, IsTernary) ++ callsCtx
    val (fields, inner) = splitNested(e)
    val accessors = cppAccessors(fields)(typesCtx, callsLocal)

    e match {
      case dict: DictNode => hint match {
        case NoHint =>
          if (isUnique(dict)) {
            val lhs = accessors.drop(1).dropRight(1)
            val rhs = run(inner)(typesCtx, callsLocal)
            s"$aggregationName.emplace($lhs, $rhs);"
          } else {
            val rhs = run(inner)(typesCtx, callsLocal)
            s"$aggregationName$accessors += $rhs;"
          }
          case VecDict =>
            val rhs = inner match { case DictNode(Seq((k, _)), VecDict) => run(k)(typesCtx, callsLocal) }
            s"$aggregationName$accessors[$rhs] += 1;"
          case Vec =>
            typesCtx(Sym(aggregationName)) match {
              case DictType(IntType, vt, Vec) if vt.isScalar =>
                val rhs = run(inner)(typesCtx, callsLocal)
                s"$aggregationName$accessors = $rhs;"
              case DictType(IntType, DictType(IntType, _, Vec), Vec) =>
                val k = (inner: @unchecked) match { case DictNode(Seq((k, Const(1))), Vec) => k }
                val rhs = run(k)(typesCtx, callsLocal)
                s"$aggregationName$accessors.emplace_back($rhs);"
              case tpe =>
                raise(s"Unexpected ${tpe.prettyPrint}")
            }
          }
      case _ => aggregation match {
        case SumAgg => s"$aggregationName += ${run(e)(typesCtx, callsLocal)};"
        case MinAgg => s"min_inplace($aggregationName, ${run(e)(typesCtx, callsLocal)});"
        case _ => raise(s"$aggregation not supported")
      }
    }
  }

  private def getAggregation(e: Exp) = e match {
    case Promote(TropicalSemiRingType(false, false, _), _) => MinAgg
    case Promote(TropicalSemiRingType(true, false, _), _) => MaxAgg
    case Promote(TropicalSemiRingType(_, true, _), _) => ProdAgg
    case _ => SumAgg
  }

  private def sumHint(e: Exp)(implicit typesCtx: TypesCtx): CodegenHint = TypeInference.run(e) match {
    case dt: DictType => getInnerDictType(dt) match { case DictType(_, _, hint) => hint }
    case _ => NoHint
  }

  @tailrec
  private def getInnerDictType(dt: DictType): DictType = dt match {
    case DictType(_, dt: DictType, _) => getInnerDictType(dt)
    case _ => dt
  }

  private def isUnique(dict: DictNode) = cond(getInnerDict(dict)) { case DictNode(Seq((_: Unique, _)), NoHint) => true }

  @tailrec
  private def getInnerDict(dict: DictNode): DictNode = dict match {
    case DictNode(Seq((_, dict: DictNode)), _) => getInnerDict(dict)
    case _ => dict
  }

  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) =
    exps.map(f => s"[${run(f)(typesCtx, callsCtx)}]").mkString("")

  private def splitNested(e: Exp): (Seq[Exp], Exp) = e match {
    case DictNode(Seq((k, v @ DictNode(_, NoHint))), _) =>
      val (lhs, rhs) = splitNested(v)
      (Seq(k) ++ lhs, rhs)
    case DictNode(Seq((k, rhs)), _) => (Seq(k), rhs)
    case _ => (Seq(), e)
  }

  private def run(e: LetBinding)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case LetBinding(x@Sym(name), e1, e2) =>
      val isTernary = !cond(e1) { case _: Sum => true }
      val localCalls = if (isTernary) Seq(IsTernary) ++ callsCtx else callsCtx
      val e1Cpp = e1 match {
        // codegen for loads was handled in a separate tree traversal
        case _: Load => ""
        case External(Limit.SYMBOL, _) => s"${run(e1)(typesCtx, Seq(LetCtx(name)) ++ localCalls)}\n"
        case c: Const => s"constexpr auto $name = ${run(c)};"
        case _ =>
          val isVector = cond(TypeInference.run(e1)) { case DictType(_, _, VecDict | Vec) => true}
          val isInitialisation = cond(e1) { case _: Sum => true }
          val cppName = if(isVector && !isInitialisation) s"&$name" else name
          s"auto $cppName = ${run(e1)(typesCtx, Seq(LetCtx(name)) ++ localCalls)};"
      }
      val e2Cpp = e2 match {
        case DictNode(Nil, _) => ""
        case _ => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), callsCtx)
      }
      e1Cpp + e2Cpp
  }

  private def run(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Sum(k, v, e1, e2) =>
      val (tpe, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val callsLocal = Seq(SumStart) ++ callsCtx
      val isLetSum = cond(callsCtx.head) { case _: LetCtx => true }
      val init = if (!isLetSum) "" else s"${cppType(tpe)} (${cppInit(tpe)});"
      val body = run(e2)(typesLocal ++ Map(Sym(aggregationName) -> tpe), callsLocal)
      e1 match {
        case _: RangeNode =>
          assert(v.name == noName)
          val n = run(e1)
          s"""$init
             |for (${cppType(IntType)} ${k.name} = 0; ${k.name} < $n; ${k.name}++) {
             |$body
             |}
             |
             |""".stripMargin
        case _ =>
          val iterable = run(e1)(typesLocal, Seq(SumEnd) ++ callsLocal)
          val head = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, NoHint) => s"&[${k.name}, ${v.name}] : $iterable"
            case DictType(_, _, VecDict | Vec) => s"&${k.name} : $iterable"
            case t => raise(s"unexpected: ${t.prettyPrint}")
          }
          s"""$init
             |for (auto $head) {
             |$body
             |}
             |""".stripMargin
      }
  }

  private def run(e: IfThenElse)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    // not
    case IfThenElse(a, Const(false), Const(true)) => s"!(${run(a)})"
    // and
    case IfThenElse(a, b, Const(false)) => s"(${run(a)} && ${run(b)})"
    // or
    case IfThenElse(a, Const(true), b) => s"(${run(a)} || ${run(b)})"

    case IfThenElse(cond, e1, e2) if checkIsTernary =>
      val callsLocal = Seq(SumEnd) ++ callsCtx
      val condBody = run(cond)(typesCtx, callsLocal)
      val ifBody = run(e1)(typesCtx, callsLocal)
      val elseBody = run(e2)(typesCtx, callsLocal)
      s"($condBody) ? $ifBody : $elseBody"

    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, Seq(SumEnd) ++ callsCtx)
      val ifBody = run(e1)
      val elseBody = e2 match {
        case DictNode(Nil, _) | Const(0) | Const(0.0) => ""
        case _ => s" else {\n${run(e2)}\n}"
      }
      s"if ($condBody) {$ifBody\n}$elseBody"
  }

  private def run(e: Cmp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Cmp(e1, e2: Sym, "∈") => TypeInference.run(e2) match {
      case _: DictType => dictCmpNil(e2, e1)
      case tpe => raise(s"expression ${e2.simpleName} should be of type " +
        s"${DictType.getClass.getSimpleName.init} not ${tpe.prettyPrint}"
      )
    }
    case Cmp(e@Get(e1, e2), DictNode(Nil, _), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
    case Cmp(DictNode(Nil, _), e@Get(e1, e2), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
    case Cmp(e1, e2, cmp) => s"${run(e1)} $cmp ${run(e2)}"
  }

  private def getArgsMatch(e: Get)(implicit typesCtx: TypesCtx) = e match {
    case Get(e1, e2) => cond(TypeInference.run(e1)) { case DictType(kt, _, _) => TypeInference.run(e2) == kt }
  }

  private def dictCmpNil(e1: Exp, e2: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) =
    TypeInference.run(e1) match {
      case DictType(IntType, _, Vec) => s"${run(e1)}[${run(e2)}] != 0"
      case _ => s"${run(e1)}.contains(${run(e2)})"
    }

  private def run(e: FieldNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case FieldNode(e1, field) =>
      val tpe = (TypeInference.run(e1): @unchecked) match { case rt: RecordType => rt }
      val idx = (tpe.indexOf(field): @unchecked) match { case Some(idx) => idx }
      s" /* $field */ std::get<$idx>(${run(e1)})"
  }

  private def run(e: Add)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Add(e1, Neg(e2)) => s"(${run(e1)} - ${run(e2)})"
    case Add(e1, e2) => s"(${run(e1)} + ${run(e2)})"
  }

  private def run(e: Mult)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Mult(e1, External(Inv.SYMBOL, args)) =>
      val divisor = args match { case Seq(divisorExp: Exp) => run(divisorExp) }
      s"(${run(e1)} / $divisor)"
    case Mult(e1, e2) =>
      s"(${run(e1)} * ${run(e2)})"
  }

  private def run(e: Neg)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = s"-${run(e)}"

  private def run(e: Sym)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match { case Sym(name) => name }

  private def run(e: DictNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case DictNode(Nil, _) =>
      ""
    case DictNode(seq, _) =>
      val localCalls = Seq(IsTernary) ++ callsCtx
      seq.map({ case (e1, e2) =>
          val e1Cpp = run(e1)(typesCtx, localCalls)
          val e2Cpp = run(e2)(typesCtx, localCalls)
          s"{$e1Cpp, $e2Cpp}"
        })
        .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")
  }

  private def run(e: RecNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case RecNode(values) =>
      val localCalls = Seq(IsTernary) ++ callsCtx
      val tpe = TypeInference.run(e)
      values.map(e => run(e._2)(typesCtx, localCalls)).mkString(s"${cppType(tpe)}(", ", ", ")")
  }

  private def run(e: Get)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Get(e1, e2) =>
      (TypeInference.run(e1): @unchecked) match {
        case _: RecordType => s"std::get<${run(e2)}>(${run(e1)})"
        case _: DictType => s"${run(e1)}[${run(e2)}]"
      }
  }

  private def run(e: External)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case External(ConstantString.SYMBOL, Seq(Const(str: String), Const(maxLen: Int))) =>
      assert(maxLen == str.length + 1)
      s"""ConstantString("$str", $maxLen)"""
    case External(StrContains.SYMBOL, Seq(str, subStr)) =>
      val func = (TypeInference.run(str), TypeInference.run(subStr)) match {
        case (StringType(None), StringType(None)) => "find"
        case (StringType(Some(_)), StringType(Some(_))) => "contains"
        case (StringType(None), StringType(Some(_))) | (StringType(Some(_)), StringType(None)) =>
          raise(s"${StrContains.SYMBOL} doesn't support fixed and variable length strings together")
      }
      s"${run(str)}.$func(${run(subStr)})"
    case External(StrStartsWith.SYMBOL, Seq(str, prefix)) =>
      val startsWith = TypeInference.run(str) match {
        case StringType(None) => "starts_with"
        case StringType(Some(_)) => "startsWith"
      }
      s"${run(str)}.$startsWith(${run(prefix)})"
    case External(StrEndsWith.SYMBOL, Seq(str, suffix)) =>
      val endsWith = TypeInference.run(str) match {
        case StringType(None) => "ends_with"
        case StringType(Some(_)) => "endsWith"
      }
      s"${run(str)}.$endsWith(${run(suffix)})"
    case External(SubString.SYMBOL, Seq(str, Const(start: Int), Const(end: Int))) =>
      val subStr = TypeInference.run(str) match {
        case StringType(None) => "substr"
        case StringType(Some(_)) => s"substr<${end - start}>"
      }
      s"${run(str)}.$subStr($start, $end)"
    case External(StrIndexOf.SYMBOL, Seq(field: FieldNode, elem, from)) =>
      assert(cond(TypeInference.run(field)) { case StringType(None) => true })
      s"${run(field)}.find(${run(elem)}, ${run(from)})"
    case External(FirstIndex.SYMBOL, Seq(on, patt)) =>
      s"${run(on)}.firstIndex(${run(patt)})"
    case External(LastIndex.SYMBOL, Seq(on, patt)) =>
      s"${run(on)}.lastIndex(${run(patt)})"
    case External(name@Inv.SYMBOL, _) =>
      raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")
    case External(MaxValue.SYMBOL, Seq(arg)) =>
      val name = (arg: @unchecked) match {
        case Sym(name) => name
      }
      s"""std::max_element($name.begin(), $name.end(), [](const auto &p1, const auto &p2) {
         |return p1.second < p2.second;
         |})->second;
         |""".stripMargin
    case External(Size.SYMBOL, Seq(arg)) =>
      TypeInference.run(arg) match {
        case _: DictType => s"${run(arg)}.size()"
        case t => raise(s"unexpected: ${t.prettyPrint}")
      }
    case External(Limit.SYMBOL, Seq(sym@Sym(arg), Const(n: Int), Const(isDescending: Boolean))) =>
      val limit = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
      val tmp = s"tmp_$uuid"
      val (dictTpe, kt, vt) = (TypeInference.run(sym): @unchecked) match {
        case dictTpe@DictType(kt, vt, _) => (dictTpe, kt, vt)
      }
      val recTpe = RecordType(Seq(Attribute("_1", kt), Attribute("_2", vt)))
      val recTpeCpp = cppType(recTpe)
      val cmp = if (isDescending) ">" else "<"
      s"""std::vector<$recTpeCpp> $tmp($n);
         |std::partial_sort_copy(
         |    $arg.begin(), $arg.end(), $tmp.begin(), $tmp.end(),
         |    []($recTpeCpp const &l, $recTpeCpp const &r) { return std::get<1>(l) $cmp std::get<1>(r); });
         |auto $limit = ${cppType(dictTpe)}($tmp.begin(), $tmp.end());
         |""".stripMargin
    case External(name, args) =>
      raise(s"unhandled function name: $name")
  }

  private def run(e: Concat)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Concat(v1@RecNode(fs1), v2@RecNode(fs2)) => run(
      {
        val (fs1m, fs2m) = fs1.toMap -> fs2.toMap
        val common =
          fs1.filter(x1 => fs2m.contains(x1._1)).map(x1 => (x1._1, x1._2, fs2m(x1._1)))
        if (common.isEmpty)
          RecNode(fs1 ++ fs2)
        else if (common.forall(x => x._2 == x._3))
          RecNode(fs1 ++ fs2.filter(x2 => !fs1m.contains(x2._1)))
        else
          raise(s"`concat($v1, $v2)` with different values for the same field name")
      }
    )
    case Concat(e1: Sym, e2: Sym) => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1: Sym, e2: RecNode) => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1: RecNode, e2: Sym) => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1, e2) =>
      val _ = TypeInference.run(e)
      raise(
        s"${Concat.getClass.getSimpleName} requires arguments " +
          s"${RecNode.getClass.getSimpleName.init} and/or ${Sym.getClass.getSimpleName.init}, " +
          s"not ${e1.simpleName} and ${e2.simpleName}"
      )
  }

  private def run(e: Const) = e match {
    case Const(DateValue(v)) =>
      val yyyymmdd = reDate.findAllIn(v.toString).matchData.next()
      s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
    case Const(v: String) =>
      s""""$v""""
    case Const(v) =>
      v.toString
  }

  private def cppInit(tpe: ir.Type): String = tpe match {
    case BoolType => "false"
    case RealType => "0.0"
    case IntType | DateType => "0"
    case StringType(None) => "\"\""
    case StringType(Some(_)) => raise("initialising VarChars shouldn't be needed")
    case DictType(_, _, NoHint | VecDict)  => "{}"
    case DictType(_, _, Vec) => vecSize.toString
    case RecordType(attrs) => attrs.map(_.tpe).map(cppInit).mkString(", ")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def cppType(tpe: ir.Type): String = tpe match {
    case BoolType => "bool"
    case RealType => "double"
    case IntType | DateType => "long"
    case StringType(None) => "std::string"
    case StringType(Some(maxLen)) => s"VarChar<$maxLen>"
    case DictType(kt, vt, NoHint) => s"phmap::flat_hash_map<${cppType(kt)}, ${cppType(vt)}>"
    case DictType(kt, IntType, VecDict) => s"vecdict<${cppType(kt)}>"
    case DictType(IntType, vt, Vec) => s"std::vector<${cppType(vt)}>"
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType).mkString("std::tuple<", ", ", ">")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def cppCsvs(exps: Seq[Exp]) = {
    val pathNameType = exps.flatMap( e =>
      iterExps(e)
        .flatMap(
          e => condOpt(e) {
            case LetBinding(Sym(name), load @ Load(path, tp: RecordType), _) if TypeInference.isColumnStore(tp) =>
              val recordType = load match { case Load(_,  recordType: RecordType) => recordType }
              (path, name, recordType)
          }
        )
    ).distinct.sortBy(_._2)

    val csvConsts = pathNameType.map({ case (path, name, _) => makeCsvConst(name, path) } ).mkString("", "\n", "\n")
    val tuples = pathNameType.map({ case (_, name, recordType) =>
      val init = makeTupleInit(name, recordType)
      s"auto ${name.toLowerCase} = ${cppType(recordType)}($init);\n"
    }).mkString("\n")

    Seq(csvConsts, tuples).mkString("\n")
  }

  private def makeCsvConst(name: String, path: String) =
    s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR);"""

  private def makeTupleInit(name: String, recordType: RecordType) = {
    assert(recordType.attrs.last.name == "size")
    val attrs = recordType.attrs.dropRight(1).map(
      attr => (attr.tpe: @unchecked) match { case DictType(IntType, vt, Vec) => Attribute(attr.name, vt) })

    (attrs.zipWithIndex.map({
      case (Attribute(attr_name, tpe), i) => s"/* $attr_name */" ++ (tpe match {
        case DateType =>
          s"dates_to_numerics(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" +  ")"
        case StringType(Some(maxLen)) =>
          s"strings_to_varchars<$maxLen>(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
        case _ =>
          s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i)"
      })
    }) ++ Seq(s"/* size */static_cast<${cppType(IntType)}>(${name.toUpperCase}_CSV.GetRowCount())"))
      .mkString(",\n")
  }

  private def iterExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: RangeNode | _: Load => Iterator()
        // 1-ary
        case Neg(e) => iterExps(e)
        case FieldNode(e, _) => iterExps(e)
        case Promote(_, e) => iterExps(e)
        case Unique(e) => iterExps(e)
        // 2-ary
        case Add(e1, e2) => iterExps(e1) ++ iterExps(e2)
        case Mult(e1, e2) => iterExps(e1) ++ iterExps(e2)
        case Cmp(e1, e2, _) => iterExps(e1) ++ iterExps(e2)
        case Sum(_, _, e1, e2) => iterExps(e1) ++ iterExps(e2)
        case Get(e1, e2) => iterExps(e1) ++ iterExps(e2)
        case Concat(e1, e2) => iterExps(e1) ++ iterExps(e2)
        case LetBinding(_, e1, e2) => iterExps(e1) ++ iterExps(e2)
        // 3-ary
        case IfThenElse(e1, e2, e3) => iterExps(e1) ++ iterExps(e2) ++ iterExps(e3)
        // n-ary
        case RecNode(values) => values.map(_._2).flatMap(iterExps)
        case DictNode(dict, _) => dict.flatMap(x => iterExps(x._1) ++ iterExps(x._2))
        case External(_, args) => args.flatMap(iterExps)
        // unhandled
        case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
      }
    )

  private def checkNoLetBindings(e: Exp)(implicit callsCtx: CallsCtx) =
    !cond(e) { case _: LetBinding => true } && !callsCtx.exists(cond(_) { case _: LetCtx  => true })

  private def checkIsSumBody(e: Exp)(implicit callsCtx: CallsCtx): Boolean = {
    !cond(e) { case _: LetBinding | _: IfThenElse | _: Sum => true } && checkActiveSumCtx
  }

  private def checkActiveSumCtx(implicit callsCtx: CallsCtx) = {
    callsCtx.indexWhere(x => cond(x) { case SumStart => true }) match {
      case -1 => false
      case start => callsCtx.indexWhere(x => cond(x) { case _: LetCtx | SumEnd => true }) match {
        case -1 => true
        case end =>
          start < end
      }
    }
  }

  private def checkIsTernary(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case IsTernary  => true })

  private def aggregationName(implicit callsCtx: CallsCtx) =
    callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head

  private def cppPrintResult(tpe: Type) = tpe match {
    case DictType(kt, vt, NoHint) =>
      s"""for (const auto &[key, val] : $resultName) {
         |$stdCout << ${_cppPrintResult(kt, "key")} << ":" << ${_cppPrintResult(vt, "val")} << std::endl;
         |}""".stripMargin
    case DictType(kt, vt, VecDict) =>
      assert(vt == IntType, s"${vt.simpleName} != ${IntType.simpleName}")
      val cond = vt match {
        case _: DictType =>
          s"!$resultName[i].empty()"
        case _: RecordType =>
          raise(s"Print not implemented for ${vt.simpleName} inside vector")
        case t =>
          assert(t.isScalar)
          s"$resultName[i] != 0"
      }
      s"""for (auto i = 0; i < $resultName.size(); ++i) {
         |if ($cond) {
         |$stdCout << ${_cppPrintResult(kt, "i")} << ":" << ${_cppPrintResult(vt, s"$resultName[i]")} << std::endl;
         |}
         |}""".stripMargin
    case _ =>
      s"$stdCout << ${_cppPrintResult(tpe, resultName)} << std::endl;"
  }

  private def _cppPrintResult(tpe: Type, name: String) = tpe match {
    case _: DictType =>
      // we currently don't pretty print inside nested dicts
      name
    case RecordType(Nil) =>
      name
    case RecordType(attrs) => attrs.zipWithIndex.map(
      {
        case (Attribute(_, tpe: RecordType), _) =>
          raise(s"Nested ${tpe.simpleName} not supported")
        case (Attribute(_, DateType), i) =>
          s"print_date(std::get<$i>($name))"
        case (Attribute(_, _), i) =>
          s"std::get<$i>($name)"
      }
    ).mkString(""""<" <<""", """ << "," << """, """<< ">"""")
    case _ =>
      assert(tpe.isScalar)
      name
  }

  private val stdCout = s"std::cout << std::setprecision (std::numeric_limits<double>::digits10)"

  private def uuid = UUID.randomUUID.toString.replace("-", "_")
}
