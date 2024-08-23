package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.backend.codegen.ChecksUtils.*
import sdql.ir.*
import sdql.raise

import scala.PartialFunction.cond
import scala.annotation.tailrec

object SumUtils {
  def run(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = e match {
    case Sum(k, v, e1, e2) =>
      val (tpe, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val callsLocal        = Seq(SumStart) ++ callsCtx
      val isLetSum          = cond(callsCtx.head) { case _: LetCtx => true }
      val agg               = getAggregation(getSumBody(e2))
      val init              = if (isLetSum) s"${cppType(tpe)} (${LLQLUtils.run(Initialise(agg, tpe))});" else ""
      val body              = CppCodegen.run(e2)(typesLocal ++ Map(Sym(aggregationName) -> tpe), callsLocal)
      val forBody = e1 match {
        case _: RangeNode => s"${cppType(IntType)} ${k.name} = 0; ${k.name} < ${CppCodegen.run(e1)}; ${k.name}++"
        case _ =>
          val iterable = CppCodegen.run(e1)(typesLocal, Seq(SumEnd) ++ callsLocal)
          val head = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, _: PHmap)         => s"&[${k.name}, ${v.name}]"
            case DictType(_, _, _: SmallVecDict)  => s"&${k.name}"
            case DictType(_, _, _: SmallVecDicts) => s"${k.name}"
            case t                                => raise(s"unexpected: ${t.prettyPrint}")
          }
          s"auto $head : $iterable"
      }
      s"$init for ($forBody) { $body }"
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
    if (isUpdate(e)) LLQLUtils.run(Update(getAggregation(e), lhs, rhs)) else LLQLUtils.run(Modify(lhs, rhs))
  }

  private def isUpdate(e: Exp)(implicit typesCtx: TypesCtx) = sumHint(e) match {
    case Some(_: PHmap) if cond(e) { case dict: DictNode => checkIsUnique(dict) } => false
    case Some(_: Vec)                                               => false
    case Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts) | None => true
  }

  private def getAggregation(e: Exp): Aggregation = e match {
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
    case dict: DictNode => TypeInference.run(dict.getInnerDict) match { case DictType(_, _, hint) => Some(hint) }
    case _              => None
  }

  private def checkIsUnique(dict: DictNode) = cond(dict.getInnerDict) {
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
    case DictNode(map, _) if map.length != 1                           => raise(s"unsupported: $e")
    case _                                                             => (Seq(), e)
  }
}
