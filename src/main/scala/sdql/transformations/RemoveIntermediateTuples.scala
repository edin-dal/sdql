package sdql.transformations

import sdql.ir.*

import scala.PartialFunction.cond

private object RemoveIntermediateTuples extends TermRewriter {
  private type ReplaceCtx = Map[Sym, RecNode]

  def apply(e: Exp): Exp = run(e)

  private def run(e: Exp)(implicit replaceCtx: ReplaceCtx = Map()): Exp = e match {
    case LetBinding(x, e1: RecNode, e2) if isSimpleRecord(e1) => run(e2)(replaceCtx ++ Map(x -> e1))

    case FieldNode(e1: Sym, field) if replaceCtx.contains(e1) =>
      (replaceCtx(e1)(field): @unchecked) match { case Some(exp) => exp }

    case e1: Sym if replaceCtx.contains(e1) => replaceCtx(e1)

    case _ => e.mapInner(run)
  }

  private def isSimpleRecord(record: RecNode) =
    record match { case RecNode(values) => values.map(_._2).forall(isSimpleField) }

  private def isSimpleField(e: Exp) = cond(e) { case Get(FieldNode(_: Sym, _), _: Sym) => true }
}
