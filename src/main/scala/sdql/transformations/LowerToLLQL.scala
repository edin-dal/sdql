package sdql.transformations

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.raise

import scala.PartialFunction.cond
import scala.annotation.tailrec

private object LowerToLLQL extends TermRewriter {
  private type TypesCtx = TypeInference.Ctx

  def apply(e: Exp): Exp = run(e)(Map(), dest = None)

  private def run(e: Exp)(implicit ctx: TypesCtx, dest: Option[Sym]): Exp = e match {
    case LetBinding(x, e1: Sum, e2) => run(LetBinding(x, sumToInitialise(e1)(ctx, Some(x)), e2))
    case LetBinding(x, e1, e2)      => LetBinding(x, e1, run(e2)(ctx ++ Map(x -> TypeInference.run(e1)), dest))
    case IfThenElse(cond, e1, e2)   => IfThenElse(cond, run(e1), run(e2))
    case Sum(key, value, e1, e2) =>
      val (_, ctxLocal) = TypeInference.sumInferTypeAndCtx(key, value, e1, e2)
      Sum(key, value, run(e1)(ctx, None), run(e2)(ctxLocal, dest))
    case _ =>
      dest match {
        case Some(dest) => sumBodyToLLQL(e, dest)(ctx)
        case None       => e.mapInner(run)
      }
  }

  private def sumBodyToLLQL(e: Exp, dest: Sym)(implicit ctx: TypesCtx) =
    if (isUpdate(e)) Update(e, Aggregation.fromExpression(e), dest) else Modify(e, dest)

  private def isUpdate(e: Exp)(implicit ctx: TypesCtx) = sumHint(e) match {
    case Some(_: PHmap) if cond(e) { case dict: DictNode => checkIsUnique(dict) } => false
    case None | Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts) => true
    case Some(_: Vec)                                               => false
  }

  private def sumHint(e: Exp)(implicit ctx: TypesCtx) = e match {
    case dict @ DictNode(map, _) if map.nonEmpty =>
      (TypeInference.run(dict.getInnerDict): @unchecked) match { case DictType(_, _, hint) => Some(hint) }
    case _ => None
  }

  private def checkIsUnique(dict: DictNode) = cond(dict.getInnerDict) {
    case DictNode(Seq((_: Unique, _)), _: PHmap) => true
  }

  private def sumToInitialise(e: Sum)(implicit ctx: TypesCtx, dest: Option[Sym]) =
    e match {
      case Sum(k, v, e1, e2) =>
        val (tpe, ctxLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
        val agg             = Aggregation.fromExpression(getSumBody(e2))
        Initialise(tpe, agg, Sum(k, v, e1, run(e2)(ctxLocal, dest)))
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
}
