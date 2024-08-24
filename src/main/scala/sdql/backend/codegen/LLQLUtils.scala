package sdql.backend.codegen

import sdql.ir.*
import sdql.raise

object LLQLUtils {
  def run(e: Initialise)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, isTernary: Boolean): String = e match {
    case Initialise(tpe, agg, e) => initialise(tpe)(agg, typesCtx, callsCtx, isTernary) ++ CppCodegen.run(e)
  }
  private def initialise(
    tpe: Type
  )(implicit agg: Aggregation, typesCtx: TypesCtx, callsCtx: CallsCtx, isTernary: Boolean): String =
    tpe match {
      case DictType(_, _, PHmap(Some(e)))                => CppCodegen.run(e)
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

  def run(e: Update)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Update(e, agg, destination) =>
      val (lhs, rhs) = cppLhsRhs(e, destination)
      agg match {
        case SumAgg => s"$lhs += $rhs;"
        case MaxAgg => s"max_inplace($lhs, $rhs);"
        case MinAgg => s"min_inplace($lhs, $rhs);"
        case agg    => raise(s"$agg not supported")
      }
  }

  def run(e: Modify)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Modify(e, destination) =>
      val (lhs, rhs) = cppLhsRhs(e, destination)
      s"$lhs = $rhs;"
  }

  private def cppLhsRhs(e: Exp, destination: String)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx) = {
    val (accessors, inner) = splitNested(e)
    val callsLocal         = Seq(SumEnd) ++ callsCtx
    val bracketed          = cppAccessors(accessors)(typesCtx, callsLocal, isTernary = true)
    val lhs                = s"$destination$bracketed"
    val rhs                = CppCodegen.run(inner)(typesCtx, callsLocal, isTernary = true)
    (lhs, rhs)
  }

  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, isTernary: Boolean) =
    exps.map(e => { s"[${CppCodegen.run(e)}]" }).mkString("")

  private def splitNested(e: Exp): (Seq[Exp], Exp) = e match {
    case DictNode(Seq((k, v @ DictNode(_, _: PHmap | _: SmallVecDict | _: SmallVecDicts))), _) =>
      val (lhs, rhs) = splitNested(v)
      (Seq(k) ++ lhs, rhs)
    case DictNode(Seq((k, DictNode(Seq((rhs, Const(1))), _: Vec))), _) => (Seq(k), rhs)
    case DictNode(Seq((k, rhs)), _)                                    => (Seq(k), rhs)
    case DictNode(map, _) if map.length != 1                           => raise(s"unsupported: $e")
    case _                                                             => (Seq(), e)
  }
}
