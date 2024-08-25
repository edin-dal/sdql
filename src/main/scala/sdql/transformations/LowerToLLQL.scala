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
        case None       => runInner(e)
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

  private def runInner(e: Exp)(implicit ctx: TypesCtx, dest: Option[Sym]) = e match {
    // 0-ary
    case _: Sym | _: Const | _: Load => e
    // 1-ary
    case Neg(e)          => Neg(run(e))
    case FieldNode(e, f) => FieldNode(run(e), f)
    case Promote(tp, e)  => Promote(tp, run(e))
    case RangeNode(e)    => RangeNode(run(e))
    case Unique(e)       => Unique(run(e))
    // 2-ary
    case Add(e1, e2)             => Add(run(e1), run(e2))
    case Mult(e1, e2)            => Mult(run(e1), run(e2))
    case Cmp(e1, e2, cmp)        => Cmp(run(e1), run(e2), cmp)
    case Sum(key, value, e1, e2) => Sum(key, value, run(e1), run(e2))
    case Get(e1, e2)             => Get(run(e1), run(e2))
    case Concat(e1, e2)          => Concat(run(e1), run(e2))
    case LetBinding(x, e1, e2)   => LetBinding(x, run(e1), run(e2))
    // 3-ary
    case IfThenElse(e1, e2, e3) => IfThenElse(run(e1), run(e2), run(e3))
    // n-ary
    case RecNode(values)      => RecNode(values.map(v => (v._1, run(v._2))))
    case DictNode(map, hint)  => DictNode(map.map(x => (run(x._1), run(x._2))), hint)
    case External(name, args) => External(name, args.map(run))
    case _                    => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }
}
