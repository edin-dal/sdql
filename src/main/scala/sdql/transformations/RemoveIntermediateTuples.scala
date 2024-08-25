package sdql.transformations

import sdql.ir.*
import sdql.raise

import scala.PartialFunction.cond
import scala.annotation.tailrec

private object RemoveIntermediateTuples extends TermRewriter {
  private type ReplaceCtx = Map[Sym, RecNode]

  def apply(e: Exp): Exp = run(e)

  @tailrec
  private def run(e: Exp)(implicit replaceCtx: ReplaceCtx = Map()): Exp = e match {
    case LetBinding(x, e1: RecNode, e2) if isSimpleRecord(e1) => run(e2)(replaceCtx ++ Map(x -> e1))

    case FieldNode(e1: Sym, field) if replaceCtx.contains(e1) =>
      (replaceCtx(e1)(field): @unchecked) match { case Some(exp) => exp }

    case e1: Sym if replaceCtx.contains(e1) => replaceCtx(e1)

    case _ => runInner(e)
  }

  private def isSimpleRecord(record: RecNode) =
    record match { case RecNode(values) => values.map(_._2).forall(isSimpleField) }

  private def isSimpleField(e: Exp) = cond(e) { case Get(FieldNode(_: Sym, _), _: Sym) => true }

  private def runInner(e: Exp)(implicit replaceCtx: ReplaceCtx) = e match {
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
    case RecNode(values)               => RecNode(values.map(v => (v._1, run(v._2))))
    case DictNode(map, PHmap(Some(e))) => DictNode(map.map(x => (run(x._1), run(x._2))), PHmap(Some(run(e))))
    case DictNode(map, hint)           => DictNode(map.map(x => (run(x._1), run(x._2))), hint)
    case External(name, args)          => External(name, args.map(run(_)))
    case _                             => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }
}
