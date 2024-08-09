package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.backend.Rewriter
import sdql.backend.codegen.ExternalFunctions
import sdql.ir.*
import sdql.ir.ExternalFunctions.{ Inv, Limit }
import sdql.{ ir, raise }

import scala.PartialFunction.{ cond, condOpt }
import scala.annotation.tailrec

sealed trait CallCtx
case class LetCtx(name: String) extends CallCtx
case object SumStart            extends CallCtx
case object SumEnd              extends CallCtx
case object IsTernary           extends CallCtx

private sealed trait Aggregation
private case object SumAgg  extends Aggregation
private case object ProdAgg extends Aggregation
private case object MinAgg  extends Aggregation
private case object MaxAgg  extends Aggregation

object CppCodegen {
  def apply(e: Exp, benchmarkRuns: Int = 0): String = {
    val rewrite   = Rewriter(e)
    val csvBody   = ReadUtils.cppCsvs(Seq(rewrite))
    val queryBody = run(rewrite)(Map(), Seq())
    val benchStart =
      if (benchmarkRuns == 0) ""
      else
        s"""HighPrecisionTimer timer;
           |for (${cppType(IntType)} iter = 1; iter <= $benchmarkRuns; iter++) {
           |timer.Reset();
           |""".stripMargin
    val benchStop =
      if (benchmarkRuns == 0) ""
      else
        s"""
           |doNotOptimiseAway($resultName);
           |timer.StoreElapsedTime(0);
           |cerr << "*" << " " << flush;
           |if (iter == $benchmarkRuns) {
           |cerr << endl;
           |std::cout << timer.GetMean(0) << " ms" << std::endl;
           |${PrintUtils.cppPrintResult(TypeInference(rewrite))}
           |}
           |}""".stripMargin
    s"""#include "../runtime/headers.h"
       |$csvBody
       |int main() {
       |$benchStart
       |$queryBody
       |$benchStop
       |${if (benchmarkRuns == 0) PrintUtils.cppPrintResult(TypeInference(rewrite)) else ""}
       |}""".stripMargin
  }

  @tailrec
  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    if (checkNoLetBindings(e)) { return run(LetBinding(Sym(resultName), e, DictNode(Nil))) }
    if (checkIsSumBody(e)) { return sumBody(e) }

    e match {
      case e: LetBinding => run(e)
      case e: Sum        => run(e)
      case e: IfThenElse => run(e)
      case e: Cmp        => run(e)
      case e: FieldNode  => run(e)
      case e: Add        => run(e)
      case e: Mult       => run(e)
      case e: Neg        => run(e)
      case e: Const      => run(e)
      case e: Sym        => run(e)
      case e: DictNode   => run(e)
      case e: RecNode    => run(e)
      case e: Get        => run(e)
      case e: External   => run(e)
      case e: Concat     => run(e)

      // ignore - handled separately in sum
      case Unique(e: Exp)     => run(e)
      case Promote(_, e: Exp) => run(e)
      case RangeNode(e: Exp)  => run(e)

      case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }
  }

  private def sumBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    val callsLocal         = Seq(SumEnd, IsTernary) ++ callsCtx
    val (accessors, inner) = splitNested(e)
    val lhs                = cppAccessors(accessors)(typesCtx, callsLocal)
    val rhs                = run(inner)(typesCtx, callsLocal)

    getAggregation(e) match {
      case SumAgg =>
        sumHint(e) match {
          case Vec =>
            typesCtx(Sym(aggregationName)) match {
              case DictType(IntType, vt, Vec) if vt.isScalar         => s"$aggregationName$lhs = $rhs;"
              case DictType(IntType, DictType(IntType, _, Vec), Vec) => s"$aggregationName$lhs.emplace_back($rhs);"
              case tpe                                               => raise(s"unexpected: ${tpe.prettyPrint}")
            }
          case NoHint if cond(e) { case dict: DictNode => isUnique(dict) } => s"$aggregationName.emplace($lhs, $rhs);"
          case NoHint | VecDict => s"$aggregationName$lhs += $rhs;"
        }
      case MinAgg => s"min_inplace($aggregationName$lhs, $rhs);"
      case agg    => raise(s"$agg not supported")
    }
  }

  private def getAggregation(e: Exp) = e match {
    case Promote(TropicalSemiRingType(false, false, _), _) => MinAgg
    case Promote(TropicalSemiRingType(true, false, _), _)  => MaxAgg
    case Promote(TropicalSemiRingType(_, true, _), _)      => ProdAgg
    case _                                                 => SumAgg
  }

  private def sumHint(e: Exp)(implicit typesCtx: TypesCtx) = e match {
    case dict: DictNode => TypeInference.run(dict.getInnerDict) match { case DictType(_, _, hint) => hint }
    case _              => NoHint
  }

  private def isUnique(dict: DictNode) = cond(dict.getInnerDict) { case DictNode(Seq((_: Unique, _)), NoHint) => true }

  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) =
    exps
      .map(e => {
        val s = run(e)(typesCtx, callsCtx)
        e match {
          case _: Unique => s
          case _         => s"[$s]"
        }
      })
      .mkString("")

  private def splitNested(e: Exp): (Seq[Exp], Exp) = e match {
    case DictNode(Seq((k, v @ DictNode(_, NoHint | VecDict))), _) =>
      val (lhs, rhs) = splitNested(v)
      (Seq(k) ++ lhs, rhs)
    case DictNode(Seq((k, DictNode(Seq((rhs, Const(1))), Vec))), _) => (Seq(k), rhs)
    case DictNode(Seq((k, rhs)), _)                                 => (Seq(k), rhs)
    case DictNode(map, _) if map.length != 1                        => raise(s"unsupported: $this")
    case _                                                          => (Seq(), e)
  }

  private def run(e: LetBinding)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case LetBinding(x @ Sym(name), e1, e2) =>
      val isTernary  = !cond(e1) { case _: Sum => true }
      val localCalls = if (isTernary) Seq(IsTernary) ++ callsCtx else callsCtx
      val e1Cpp = e1 match {
        // codegen for loads was handled in a separate tree traversal
        case _: Load                   => ""
        case External(Limit.SYMBOL, _) => s"${run(e1)(typesCtx, Seq(LetCtx(name)) ++ localCalls)}\n"
        case c: Const                  => s"constexpr auto $name = ${run(c)};"
        case _ =>
          val isInitialisation = cond(e1) { case _: Sum => true }
          val cppName          = if (!isInitialisation && isVector(e1)) s"&$name" else name
          s"auto $cppName = ${run(e1)(typesCtx, Seq(LetCtx(name)) ++ localCalls)};"
      }
      val e2Cpp = e2 match {
        case DictNode(Nil, _) => ""
        case _                => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), callsCtx)
      }
      e1Cpp + e2Cpp
  }

  private def isVector(e: Exp)(implicit typesCtx: TypesCtx) =
    cond(TypeInference.run(e)) { case DictType(_, _, VecDict | Vec) => true }

  private def run(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Sum(k, v, e1, e2) =>
      val (tpe, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val callsLocal        = Seq(SumStart) ++ callsCtx
      val isLetSum          = cond(callsCtx.head) { case _: LetCtx => true }
      val init              = if (!isLetSum) "" else s"${cppType(tpe)} (${cppInit(tpe)});"
      val body              = run(e2)(typesLocal ++ Map(Sym(aggregationName) -> tpe), callsLocal)
      e1 match {
        case _: RangeNode =>
          assert(v.name == "_")
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
            case DictType(_, _, NoHint)        => s"&[${k.name}, ${v.name}] : $iterable"
            case DictType(_, _, VecDict | Vec) => s"&${k.name} : $iterable"
            case t                             => raise(s"unexpected: ${t.prettyPrint}")
          }
          s"""$init
             |for (auto $head) {
             |$body
             |}
             |""".stripMargin
      }
  }

  private def run(e: IfThenElse)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case IfThenElse(a, Const(false), Const(true)) => s"!(${run(a)})"
    case IfThenElse(a, b, Const(false))           => s"(${run(a)} && ${run(b)})"
    case IfThenElse(a, Const(true), b)            => s"(${run(a)} || ${run(b)})"
    case _ if checkIsTernary                      => ternary(e)
    case _                                        => default(e)
  }
  private def ternary(e: IfThenElse)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) = e match {
    case IfThenElse(cond, e1, e2) =>
      val callsLocal = Seq(SumEnd) ++ callsCtx
      val condBody   = run(cond)(typesCtx, callsLocal)
      val ifBody     = run(e1)(typesCtx, callsLocal)
      val elseBody   = run(e2)(typesCtx, callsLocal)
      s"($condBody) ? $ifBody : $elseBody"
  }
  private def default(e: IfThenElse)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) = e match {
    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, Seq(SumEnd) ++ callsCtx)
      val ifBody   = run(e1)
      val elseBody = e2 match {
        case DictNode(Nil, _) | Const(0) | Const(0.0) => ""
        case _                                        => s" else {\n${run(e2)}\n}"
      }
      s"if ($condBody) {$ifBody\n}$elseBody"
  }

  private def run(e: Cmp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Cmp(e1, e2: Sym, "∈") =>
      TypeInference.run(e2) match {
        case _: DictType => dictCmpNil(e2, e1)
        case tpe =>
          raise(
            s"expression ${e2.simpleName} should be of type " +
              s"${DictType.getClass.getSimpleName.init} not ${tpe.prettyPrint}"
          )
      }
    case Cmp(e @ Get(e1, e2), DictNode(Nil, _), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
    case Cmp(DictNode(Nil, _), e @ Get(e1, e2), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
    case Cmp(e1, e2, cmp)                                                => s"${run(e1)} $cmp ${run(e2)}"
  }
  private def getArgsMatch(e: Get)(implicit typesCtx: TypesCtx) = e match {
    case Get(e1, e2) => cond(TypeInference.run(e1)) { case DictType(kt, _, _) => TypeInference.run(e2) == kt }
  }
  private def dictCmpNil(e1: Exp, e2: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) =
    TypeInference.run(e1) match {
      case DictType(IntType, _, Vec) => s"${run(e1)}[${run(e2)}] != 0"
      case _                         => s"${run(e1)}.contains(${run(e2)})"
    }

  private def run(e: FieldNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case FieldNode(e1, field) =>
      val tpe = (TypeInference.run(e1): @unchecked) match { case rt: RecordType => rt }
      val idx = (tpe.indexOf(field): @unchecked) match { case Some(idx)         => idx }
      s" /* $field */ std::get<$idx>(${run(e1)})"
  }

  private def run(e: Add)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Add(e1, Neg(e2)) => s"(${run(e1)} - ${run(e2)})"
    case Add(e1, e2)      => s"(${run(e1)} + ${run(e2)})"
  }

  private def run(e: Mult)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Mult(e1, External(Inv.SYMBOL, Seq(e2))) => s"(${run(e1)} / ${run(e2)})"
    case Mult(e1, e2)                            => s"(${run(e1)} * ${run(e2)})"
  }

  private def run(e: Neg)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = s"-${run(e)}"

  private def run(e: Sym): String = e match { case Sym(name) => name }

  private def run(e: DictNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case DictNode(Nil, _) => ""
    case DictNode(seq, _) =>
      val localCalls = Seq(IsTernary) ++ callsCtx
      seq
        .map({ case (e1, e2) => s"{${run(e1)(typesCtx, localCalls)}, ${run(e2)(typesCtx, localCalls)}}" })
        .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")
  }

  private def run(e: RecNode)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case RecNode(values) =>
      val localCalls = Seq(IsTernary) ++ callsCtx
      val tpe        = TypeInference.run(e)
      values.map(e => run(e._2)(typesCtx, localCalls)).mkString(s"${cppType(tpe)}(", ", ", ")")
  }

  private def run(e: Get)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Get(e1, e2) =>
      (TypeInference.run(e1): @unchecked) match {
        case _: RecordType => s"std::get<${run(e2)}>(${run(e1)})"
        case _: DictType   => s"${run(e1)}[${run(e2)}]"
      }
  }

  private def run(e: External)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = ExternalFunctions.run(e)

  private def run(e: Concat)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Concat(e1: RecNode, e2: RecNode) => run(e1.concat(e2))
    case Concat(e1: Sym, e2: Sym)         => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1: Sym, e2: RecNode)     => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1: RecNode, e2: Sym)     => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
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
      val yyyymmdd = "^(\\d{4})(\\d{2})(\\d{2})$".r.findAllIn(v.toString).matchData.next()
      s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
    case Const(v: String) => s""""$v""""
    case Const(v)         => v.toString
  }

  private def cppInit(tpe: ir.Type): String = tpe match {
    case BoolType                         => "false"
    case RealType                         => "0.0"
    case IntType | DateType               => "0"
    case StringType(None)                 => "\"\""
    case StringType(Some(_))              => raise("initialising VarChars shouldn't be needed")
    case DictType(_, _, NoHint | VecDict) => "{}"
    case DictType(_, _, Vec)              => "DEFAULT_VEC_SIZE"
    case RecordType(attrs)                => attrs.map(_.tpe).map(cppInit).mkString(", ")
    case tpe                              => raise(s"unimplemented type: $tpe")
  }

  private def checkNoLetBindings(e: Exp)(implicit callsCtx: CallsCtx) =
    !cond(e) { case _: LetBinding => true } && !callsCtx.exists(cond(_) { case _: LetCtx => true })

  private def checkIsSumBody(e: Exp)(implicit callsCtx: CallsCtx) =
    !cond(e) { case _: LetBinding | _: IfThenElse | _: Sum => true } && checkActiveSumCtx

  private def checkActiveSumCtx(implicit callsCtx: CallsCtx) =
    callsCtx.indexWhere(x => cond(x) { case SumStart => true }) match {
      case -1 => false
      case start =>
        callsCtx.indexWhere(x => cond(x) { case _: LetCtx | SumEnd => true }) match {
          case -1  => true
          case end => start < end
        }
    }

  private def checkIsTernary(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case IsTernary => true })

  private def aggregationName(implicit callsCtx: CallsCtx) =
    callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
}
