package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.backend.codegen.ChecksUtils.aggregationName
import sdql.ir.*
import sdql.raise

import scala.PartialFunction.cond

object SumUtils {
  def run(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, isTernary: Boolean): String = e match {
    case Sum(k, v, e1, e2) =>
      val (tpe, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val callsLocal        = Seq(SumStart) ++ callsCtx
      val body              = CppCodegen.run(e2)(typesLocal ++ Map(Sym(aggregationName) -> tpe), callsLocal, isTernary)
      val forBody = e1 match {
        case _: RangeNode => s"${cppType(IntType)} ${k.name} = 0; ${k.name} < ${CppCodegen.run(e1)}; ${k.name}++"
        case _ =>
          val iterable = CppCodegen.run(e1)(typesLocal, Seq(SumEnd) ++ callsLocal, isTernary)
          val head = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, _: PHmap)         => s"&[${k.name}, ${v.name}]"
            case DictType(_, _, _: SmallVecDict)  => s"&${k.name}"
            case DictType(_, _, _: SmallVecDicts) => s"${k.name}"
            case t                                => raise(s"unexpected: ${t.prettyPrint}")
          }
          s"auto $head : $iterable"
      }
      s"for ($forBody) { $body }"
  }

  def sumBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String =
    if (isUpdate(e)) LLQLUtils.run(Update(e, getAggregation(e), aggregationName))
    else LLQLUtils.run(Modify(e, aggregationName))

  private def isUpdate(e: Exp)(implicit typesCtx: TypesCtx) = sumHint(e) match {
    case Some(_: PHmap) if cond(e) { case dict: DictNode => checkIsUnique(dict) } => false
    case None | Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts) => true
    case Some(_: Vec)                                               => false
  }

  def getAggregation(e: Exp): Aggregation = e match {
    case Promote(tp, _) => getAggregation(tp)
    case _              => SumAgg
  }

  def getAggregation(tp: Type): Aggregation = tp match {
    case TropicalSemiRingType(false, false, _) => MinAgg
    case TropicalSemiRingType(true, false, _)  => MaxAgg
    case TropicalSemiRingType(_, true, _)      => ProdAgg
    case _                                     => raise(s"unexpected: ${tp.prettyPrint}")
  }

  private def sumHint(e: Exp)(implicit typesCtx: TypesCtx) = e match {
    case dict: DictNode =>
      (TypeInference.run(dict.getInnerDict): @unchecked) match { case DictType(_, _, hint) => Some(hint) }
    case _ => None
  }

  private def checkIsUnique(dict: DictNode) = cond(dict.getInnerDict) {
    case DictNode(Seq((_: Unique, _)), _: PHmap) => true
  }
}
