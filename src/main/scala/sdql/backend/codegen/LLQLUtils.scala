package sdql.backend.codegen

import sdql.ir.*
import sdql.raise

object LLQLUtils {
  def run(e: Initialise)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Initialise(agg, tpe) => initialise(tpe)(agg, typesCtx, callsCtx)
  }
  private def initialise(tpe: Type)(implicit agg: Aggregation, typesCtx: TypesCtx, callsCtx: CallsCtx): String =
    tpe match {
      case DictType(_, _, PHmap(Some(e)))                => initialisePHMap(e)(typesCtx, callsCtx)
      case DictType(_, _, PHmap(None) | _: SmallVecDict) => "{}"
      case DictType(_, _, Vec(size)) =>
        size match {
          case None       => ""
          case Some(size) => (size + 1).toString
        }
      case DictType(_, _, _: SmallVecDicts) => ""
      case RecordType(attrs)                => attrs.map(_.tpe).map(initialise).mkString(", ")
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
  private def initialisePHMap(e: Exp)(typesCtx: TypesCtx, callsCtx: CallsCtx) = e match {
    // we can't generally handle sums inline - this is special handling for handy use cases
    case Sum(Sym(k), _, e1: DictNode, e2)
        if CppCodegen.run(e2)(typesCtx, callsCtx) == k && SumUtils.getAggregation(e2) == MinAgg =>
      s"min_key(${CppCodegen.run(e1)(typesCtx, callsCtx)})"
    case Sum(Sym(k), _, e1: DictNode, e2)
        if CppCodegen.run(e2)(typesCtx, callsCtx) == k && SumUtils.getAggregation(e2) == MaxAgg =>
      s"max_key(${CppCodegen.run(e1)(typesCtx, callsCtx)})"
    case _ => CppCodegen.run(e)(typesCtx, callsCtx)
  }

  def run(e: Update): String = e match {
    case Update(agg, hint, isUnique, lhs, rhs) =>
      agg match {
        case SumAgg =>
          hint match {
            case Some(_: PHmap) if isUnique                                 => s"$lhs = $rhs;"
            case None | Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts) => s"$lhs += $rhs;"
            case Some(_: Vec)                                               => s"$lhs = $rhs;"
          }
        case MaxAgg => s"max_inplace($lhs, $rhs);"
        case MinAgg => s"min_inplace($lhs, $rhs);"
        case agg    => raise(s"$agg not supported")
      }
  }
}
