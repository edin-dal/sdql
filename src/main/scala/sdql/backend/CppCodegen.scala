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
private case class SumCtx(on: String, k: String, v: String) extends CallCtx
private case class SumEnd() extends CallCtx
private case class IsTernary() extends CallCtx
private case class Promoted(isMax: Boolean, isProd: Boolean) extends CallCtx

object CppCodegen {
  private type TypesCtx = TypeInference.Ctx
  private type CallsCtx = List[CallCtx]

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
    val queryBody = run(rewrite)(Map(), List())
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
    val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
    val hint = sumHint(e)
    val isMin = e match {
      case Promote(TropicalSemiRingType(isMax, false, RealType), _) => !isMax
      case _ => false
    }
    val promo = e
    val promoCtx = e match {
      case Promote(TropicalSemiRingType(isMax, isProd, _), _) => List(Promoted(isMax, isProd))
      case IfThenElse(_, Promote(TropicalSemiRingType(isMax, isProd, _), _), _) => List(Promoted(isMax, isProd))
      case IfThenElse(_, _, Promote(TropicalSemiRingType(isMax, isProd, _), _)) => List(Promoted(isMax, isProd))
      case _ => List()
    }
    val callsLocal = List(SumEnd(), IsTernary()) ++ promoCtx ++ callsCtx

    // TODO get rid of hack for job/gj queries
    if (cond(hint) { case VecDict() => true } && cond(promo) { case _: DictNode => true }) {
      val dict = (promo: @unchecked) match { case dt: DictNode => dt }
      val tpe = typesCtx(Sym(agg))
      if (cond(tpe) { case dt: DictType => isNestedRecordToInt(dt) && agg.startsWith("interm") }) {
        val (fields, _) = splitNested(dict)
        val accessors = cppAccessors(fields)(typesCtx, callsLocal)
        val cols = getRecordType(tpe) match {
          case Some(RecordType(attrs)) => attrs.map(_.name)
        }
        val exps = getRecordNode(dict) match {
          case RecNode(values) => values.map(_._2)
        }
        val expsCpp = exps.map(run(_)(typesCtx, callsLocal))
        val insertions = cols
          .zip(expsCpp).map { case (col, cpp) => s"${agg}_inner.$col.push_back($cpp);" }.mkString("\n")
        val link = s"$agg$accessors[${agg}_cnt++] += 1;"
        return s"""$insertions
             |$link
             |""".stripMargin
      }
    }

    promo match {
      case dict@DictNode(seq, _) => seq.map(
        kv => {
          val lhs = run(kv._1)(typesCtx, callsLocal)
          val rhs = run(kv._2)(typesCtx, callsLocal)
          if (cond(kv._1) { case _: Unique => true })
            s"$agg.emplace($lhs, $rhs);"
          else hint match {
            case _: NoHint =>
              val (fields, inner) = splitNested(dict)
              val rhs = run(inner)(typesCtx, callsLocal)
              val accessors = cppAccessors(fields)(typesCtx, callsLocal)
              s"$agg$accessors += $rhs;"
            case _: VecDict =>
              typesCtx(Sym(agg)) match {
                case dt: DictType if isNestedRecordToInt(dt) =>
                  val (fields, _) = splitNested(dict)
                  val accessors = cppAccessors(fields)(typesCtx, callsLocal)
                  s"$agg$accessors[${sumVariable(callsLocal)}] += 1;"
                case tpe =>
                  raise(s"Unexpected ${tpe.prettyPrint}")
              }
            case Vector() =>
              typesCtx(Sym(agg)) match {
                case DictType(IntType, vt, Vector()) if vt.isScalar =>
                  s"$agg[$lhs] = $rhs;"
                case DictType(IntType, DictType(IntType, _, Vector()), Vector()) =>
                  val (fields, inner) = splitNested(dict)
                  val lhs = cppAccessors(fields)(typesCtx, callsLocal)
                  val k = (inner: @unchecked) match { case DictNode(Seq((k, Const(1))), Vector()) => k }
                  val rhs = run(k)(typesCtx, callsLocal)
                  s"$agg$lhs.emplace_back($rhs);"
                case tpe =>
                  raise(s"Unexpected ${tpe.prettyPrint}")
              }
          }
        }
      ).mkString("\n")
      case _ if isMin =>
        s"min_inplace($agg, ${run(promo)(typesCtx, callsLocal)});"
      case _ =>
        s"$agg += ${run(promo)(typesCtx, callsLocal)};"
    }
  }

  private def run(e: LetBinding)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case LetBinding(x@Sym(name), e1, e2) =>
      val isTernary = !cond(e1) { case _: Sum => true }
      val localCalls = if (isTernary) List(IsTernary()) ++ callsCtx else callsCtx
      val e1Cpp = e1 match {
        // codegen for loads was handled in a separate tree traversal
        case _: Load => ""
        case External(Limit.SYMBOL, _) => s"${run(e1)(typesCtx, List(LetCtx(name)) ++ localCalls)}\n"
        case c: Const => s"constexpr auto $name = ${run(c)};"
        case _ =>
          // if it's a vector take a reference rather than copy
          val cppName = if (isRecordToInt(TypeInference.run(e1))) s"&$name" else name
          s"auto $cppName = ${run(e1)(typesCtx, List(LetCtx(name)) ++ localCalls)};"
      }
      val e2Cpp = e2 match {
        case DictNode(Nil, _) => ""
        case _ => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), callsCtx)
      }
      e1Cpp + e2Cpp
  }

  private def run(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Sum(k, v, e1, e2) =>
      val hint = sumHint(e1)

      val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
      var (tpe, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      typesLocal ++= Map(Sym(agg) -> tpe)

      val on = e1 match {
        case Sym(name) => name
        case Get(Sym(name), _) => name
        case _ => ""
      }
      val callsLocal = List(SumCtx(on, k = k.name, v = v.name)) ++ callsCtx

      val isNestedSum = inferSumNesting(callsLocal) > 0
      val isLetSum = cond(callsCtx.head) { case _: LetCtx => true }
      val init = if (isNestedSum && !isLetSum) "" else s"${cppType(tpe)} (${cppInit(tpe)});"

      val body = run(e2)(typesLocal, callsLocal)

      if (cond(e1) { case _: RangeNode => true }) {
        val n = run(e1)
        assert(v.name == noName)
        s"""$init
           |for (${cppType(IntType)} ${k.name} = 0; ${k.name} < $n; ${k.name}++) {
           |$body
           |}
           |
           |""".stripMargin
      } else {
        val isVecSum = cond(hint) { case VecDict() => true }
        val initBlock = getRecordType(tpe) match {
          case Some(RecordType(attrs)) if isVecSum && !isNestedSum =>
            val aggVectors = attrs.map(attr => s"std::vector<${cppType(attr.tpe)}> ${attr.name};")
              .mkString(s"struct ${agg.toUpperCase}_TYPE {", "\n", s"} ${agg}_inner;")
            s"""$init
               |$aggVectors
               |auto ${agg}_cnt = 0;
               |""".stripMargin
          case _ =>
            init
        }
        val iterable = run(e1)(typesLocal, List(SumEnd()) ++ callsLocal)
        val head = TypeInference.run(e1)(typesLocal) match {
          case DictType(_, _, NoHint()) =>
            s"[${k.name}, ${v.name}] : $iterable"
          case DictType(_, _, VecDict()) =>
            s"${k.name}_i : $iterable"
        }
        s"""$initBlock
           |for (auto &$head) {
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
      val callsLocal = List(SumEnd()) ++ callsCtx
      val condBody = run(cond)(typesCtx, callsLocal)
      val ifBody = run(e1)(typesCtx, callsLocal)
      val elseBody = run(e2)(typesCtx, callsLocal)
      s"($condBody) ? $ifBody : $elseBody"

    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, List(SumEnd()) ++ callsCtx)
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
      case DictType(IntType, _, Vector()) => s"${run(e1)}[${run(e2)}] != 0"
      case _ => s"${run(e1)}.contains(${run(e2)})"
    }

  private def run(e: FieldNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case FieldNode(e1, field) =>
      val tpe = (TypeInference.run(e1): @unchecked) match { case rt: RecordType => rt }
      val idx = (tpe.indexOf(field): @unchecked) match { case Some(idx) => idx }

      // TODO get rid of hack for job/gj queries
      e1 match {
        case Sym(name) =>
          if (name.endsWith("_tuple") && !name.startsWith("interm")) {
            val origin = name.dropRight("_tuple".length)
            return s" /* $field */ std::get<$idx>($origin)[${name}_i]"
          }
          if (
            name.endsWith("_tuple") && !checkIsMin &&
              cond(TypeInference.run(Sym(getSumOrigin(name)))) { case dt: DictType => isNestedRecordToInt(dt) }) {
            val origin = getOrigin(name)
            return s"${origin}_trie0_inner.$field[${origin}_tuple_i]"
          }
          if (!name.startsWith("mn_") && checkIsMin) {
            val origin = getOrigin(name)
            return s"${origin}_trie0_inner.$field[${origin}_tuple_i]"
          }
        case _ =>
      }

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
      val localCalls = List(IsTernary()) ++ callsCtx
      seq.map({ case (e1, e2) =>
          val e1Cpp = run(e1)(typesCtx, localCalls)
          val e2Cpp = run(e2)(typesCtx, localCalls)
          s"{$e1Cpp, $e2Cpp}"
        })
        .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")
  }

  private def run(e: RecNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case RecNode(values) =>
      val localCalls = List(IsTernary()) ++ callsCtx
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

  private def sumHint(e: Exp)(implicit typesCtx: TypesCtx): CodegenHint = e match {
    case _: RangeNode => NoHint()
    case _ => TypeInference.run(e) match {
      case dt: DictType => getInnerDict(dt) match {
        case DictType(_, _, hint) => hint
      }
      case _ => NoHint()
    }
  }

  @tailrec
  private def getInnerDict(dt: DictType): DictType = dt match {
    case DictType(_, dt: DictType, _) => getInnerDict(dt)
    case _ => dt
  }

  @tailrec
  private def getRecordType(tpe: Type): Option[RecordType] = tpe match {
    case DictType(_, dt: DictType, _) => getRecordType(dt)
    case DictType(rt: RecordType, IntType, _) => Some(rt)
    case _ => None
  }

  @tailrec
  private def isNestedRecordToInt(tp: Type): Boolean = tp match {
    case DictType(_, dt: DictType, _) => isNestedRecordToInt(dt)
    case _ => isRecordToInt(tp)
  }

  private def isRecordToInt(tp: Type): Boolean = tp match {
    case DictType(_: RecordType, IntType, VecDict()) => true
    case _ => false
  }

  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) =
    exps.map(f => s"[${run(f)(typesCtx, callsCtx)}]").mkString("")

  private def splitNested(e: Exp): (Seq[Exp], Exp) = e match {
    case DictNode(Seq((k, v @ DictNode(_, NoHint()))), _) =>
      val (lhs, rhs) = splitNested(v)
      (Seq(k) ++ lhs, rhs)
    case DictNode(Seq((k, rhs)), _) => (Seq(k), rhs)
    case _ => (Seq(), e)
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
    case DictType(_, _, NoHint()) => "{}"
    case DictType(_, _, Vector()) => vecSize.toString
    case RecordType(attrs) => attrs.map(_.tpe).map(cppInit).mkString(", ")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def cppType(tpe: ir.Type): String = tpe match {
    case BoolType => "bool"
    case RealType => "double"
    case IntType | DateType => "long"
    case StringType(None) => "std::string"
    case StringType(Some(maxLen)) => s"VarChar<$maxLen>"
    case DictType(kt, vt, NoHint()) => s"phmap::flat_hash_map<${cppType(kt)}, ${cppType(vt)}>"
    case DictType(_: RecordType, vt, VecDict()) => s"vecdict<${cppType(vt)}>"
    case DictType(IntType, vt, Vector()) => s"std::vector<${cppType(vt)}>"
    case _: DictType => raise(s"unexpected type: ${tpe.prettyPrint}")
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
      val tupleType = "std::tuple" // FIXME cppType(recordType)
      s"auto ${name.toLowerCase} = $tupleType($init);\n"
    }).mkString("\n")

    List(csvConsts, tuples).mkString("\n")
  }

  private def makeCsvConst(name: String, path: String) =
    s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR);"""

  private def makeTupleInit(name: String, recordType: RecordType) = {
    (recordType.attrs.zipWithIndex.map(
        {
          case (Attribute(attr_name, tpe), i) => s"/* $attr_name */" ++ (tpe match {
            case innerType @ IntType if attr_name == "size" =>
              s"static_cast<${cppType(innerType)}>(${name.toUpperCase}_CSV.GetRowCount())"
            case DictType(IntType, DateType, VecDict()) =>
              val tpe = StringType()
              s"dates_to_numerics(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i)" +  "),"
            case DictType(IntType, StringType(Some(maxLen)), VecDict()) =>
              val innerType = StringType()
              s"strings_to_varchars<$maxLen>(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(innerType)}>($i)" + "),"
            case DictType(IntType, innerType, VecDict()) =>
              s"${name.toUpperCase}_CSV.GetColumn<${cppType(innerType)}>($i),"
          })
        }
      ) ++ Seq())
      .mkString("\n")
  }

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
        case Unique(e) =>
          iterExps(e)
        // 2-ary
        case Add(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Mult(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Cmp(e1, e2, _) =>
          iterExps(e1) ++ iterExps(e2)
        case Sum(_, _, e1, e2) =>
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
        case DictNode(dict, _) =>
          dict.flatMap(x => iterExps(x._1) ++ iterExps(x._2)).toList
        case External(_, args) =>
          args.flatMap(iterExps).toList
        // unhandled
        case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
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

  private def checkIsMin(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case Promoted(isMax, _)  => !isMax })

  private def checkIsTernary(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case _: IsTernary  => true })

  private def sumVariable(implicit callsCtx: CallsCtx) = ('i'.toInt + inferSumNesting).toChar

  private def inferSumNesting(implicit callsCtx: CallsCtx) =
    (callsCtx.takeWhile(!cond(_) { case _: SumEnd => true }).count(cond(_) { case _: SumCtx => true }) - 1).max(0)

  private def cppPrintResult(tpe: Type) = tpe match {
    case DictType(kt, vt, NoHint()) =>
      s"""for (const auto &[key, val] : $resultName) {
         |$stdCout << ${_cppPrintResult(kt, "key")} << ":" << ${_cppPrintResult(vt, "val")} << std::endl;
         |}""".stripMargin
    case DictType(kt, vt, VecDict()) =>
      assert(kt == IntType, s"${kt.simpleName} != ${IntType.simpleName}")
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

  // TODO get rid of hack for job/gj queries
  @tailrec
  private def getRecordNode(dict: DictNode)(implicit typesCtx: TypesCtx): RecNode = dict match {
    case DictNode(Seq((_, v: DictNode)), _) => getRecordNode(v)
    case DictNode(Seq((record: RecNode, Const(1))), _) => record
    case _ => raise(s"unexpected: ${TypeInference.run(dict).prettyPrint}")
  }
  @tailrec
  private def getOrigin(name: String)(implicit callsCtx: CallsCtx): String = {
    val sumOrigin = getSumOrigin(name)
    val origin = reOrigin.findAllIn(sumOrigin).matchData.next()
    val n = origin.group("n").toInt
    val suffix: String = origin.group("suffix")
    if (n == 0) suffix else getOrigin(s"${suffix}_trie${n - 1}")
  }
  @tailrec
  private def getSumOrigin(name: String)(implicit callsCtx: CallsCtx): String =
    callsCtx.flatMap(condOpt(_) { case SumCtx(origin, k, v) if name == k || name == v => origin }).headOption match {
      case Some(origin) => getSumOrigin(origin)
      case None => name
    }
  private val reOrigin = "(?<suffix>.*)_trie(?<n>\\d+)$".r
}
