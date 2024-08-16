package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.backend.codegen.ChecksUtils.*
import sdql.ir.*
import sdql.raise

import scala.PartialFunction.cond
import scala.annotation.tailrec

private sealed trait Aggregation
private case object SumAgg  extends Aggregation
private case object ProdAgg extends Aggregation
private case object MinAgg  extends Aggregation
private case object MaxAgg  extends Aggregation

object SumUtils {
  def run(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Sum(k, v, e1, e2) =>
      val (tpe, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val callsLocal        = Seq(SumStart) ++ callsCtx
      val isLetSum          = cond(callsCtx.head) { case _: LetCtx => true }
      val agg               = getAggregation(getSumBody(e2))
      val init              = if (isLetSum) s"${cppType(tpe)} (${cppInit(tpe)(agg, typesCtx, callsCtx)});" else ""
      val body              = CppCodegen.run(e2)(typesLocal ++ Map(Sym(aggregationName) -> tpe), callsLocal)
      e1 match {
        case _: RangeNode =>
          assert(v.name == "_")
          val n = CppCodegen.run(e1)
          s"""$init
             |for (${cppType(IntType)} ${k.name} = 0; ${k.name} < $n; ${k.name}++) {
             |$body
             |}
             |
             |""".stripMargin
        case _ =>
          val iterable = CppCodegen.run(e1)(typesLocal, Seq(SumEnd) ++ callsLocal)
          val head = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, _: PHmap)         => s"&[${k.name}, ${v.name}]"
            case DictType(_, _, _: SmallVecDicts) => s"${k.name}"
            case _                                => s"&${k.name}"
          }
          s"""$init
             |for (auto $head : $iterable) {
             |$body
             |}
             |""".stripMargin
      }
  }

  @tailrec
  private def getSumBody(e: Exp): Exp = e match {
    case Sum(_, _, _, e2)                                  => getSumBody(e2)
    case LetBinding(_, _, e2)                              => getSumBody(e2)
    case IfThenElse(_, DictNode(Nil, _), DictNode(Nil, _)) => raise("both branches empty")
    case IfThenElse(_, DictNode(Nil, _), e2)               => getSumBody(e2)
    case IfThenElse(_, e1, DictNode(Nil, _))               => getSumBody(e1)
    case IfThenElse(_, e1, _)                              => getSumBody(e1) // pick arbitrary branch
    case _                                                 => e
  }

  def sumBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    val callsLocal         = Seq(SumEnd, IsTernary) ++ callsCtx
    val (accessors, inner) = splitNested(e)
    val bracketed          = cppAccessors(accessors)(typesCtx, callsLocal)
    val lhs                = s"$aggregationName$bracketed"
    val rhs                = CppCodegen.run(inner)(typesCtx, callsLocal)

    getAggregation(e) match {
      case SumAgg =>
        sumHint(e) match {
          case None | Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts) if !cond(e) {
                case dict: DictNode => isUnique(dict)
              } =>
            s"$lhs += $rhs;"
          case _ => s"$lhs = $rhs;"
        }
      case MinAgg => s"min_inplace($lhs, $rhs);"
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
    case dict: DictNode => TypeInference.run(dict.getInnerDict) match { case DictType(_, _, hint) => Some(hint) }
    case _              => None
  }

  private def isUnique(dict: DictNode) = cond(dict.getInnerDict) {
    case DictNode(Seq((_: Unique, _)), _: PHmap) => true
  }

  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) =
    exps.map(e => { s"[${CppCodegen.run(e)(typesCtx, callsCtx)}]" }).mkString("")

  private def splitNested(e: Exp): (Seq[Exp], Exp) = e match {
    case DictNode(Seq((k, v @ DictNode(_, _: PHmap | _: SmallVecDict | _: SmallVecDicts))), _) =>
      val (lhs, rhs) = splitNested(v)
      (Seq(k) ++ lhs, rhs)
    case DictNode(Seq((k, DictNode(Seq((rhs, Const(1))), _: Vec))), _) => (Seq(k), rhs)
    case DictNode(Seq((k, rhs)), _)                                    => (Seq(k), rhs)
    case DictNode(map, _) if map.length != 1                           => raise(s"unsupported: $this")
    case _                                                             => (Seq(), e)
  }

  private def cppInit(tpe: Type)(implicit agg: Aggregation, typesCtx: TypesCtx, callsCtx: CallsCtx): String =
    tpe match {
      case DictType(_, _, PHmap(Some(e)))                => CppCodegen.run(e)
      case DictType(_, _, PHmap(None) | _: SmallVecDict) => "{}"
      case DictType(_, _, Vec(size)) =>
        size match {
          case None       => ""
          case Some(size) => (size + 1).toString
        }
      case DictType(_, _, _: SmallVecDicts) => ""
      case RecordType(attrs)                => attrs.map(_.tpe).map(cppInit).mkString(", ")
      case BoolType =>
        agg match {
          case SumAgg | MaxAgg  => "false"
          case ProdAgg | MinAgg => "true"
        }
      case RealType =>
        agg match {
          case SumAgg | MaxAgg => "0.0"
          case ProdAgg         => "1.0"
          case MinAgg          => s"std::numeric_limits<${cppType(RealType)}>::max()"
        }
      case IntType | DateType =>
        agg match {
          case SumAgg | MaxAgg => "0"
          case ProdAgg         => "1"
          case MinAgg          => s"std::numeric_limits<${cppType(IntType)}>::max()"
        }
      case StringType(None) =>
        agg match {
          case SumAgg | MaxAgg => "\"\""
          case ProdAgg         => raise("undefined")
          case MinAgg          => s"MAX_STRING"
        }
      case StringType(Some(_)) => raise("initialising VarChars shouldn't be needed")
      case tpe                 => raise(s"unimplemented type: $tpe")
    }
}
