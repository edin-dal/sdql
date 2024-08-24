package sdql.transformations

import sdql.ir.*

private trait TermRewriter { def apply(e: Exp): Exp }

object Rewriter {
  private val rewriters = Seq(
    RemoveAliases,
    RemoveRecordGet,
    SkipUnusedColumns,
    RemoveIntermediateTuples,
    BindFreeExpression,
  )

  def apply(e: Exp): Exp = rewriters.foldLeft(e) { (acc, f) =>
    f(acc)
  }

  def toLLQL: Exp => Exp = LowerToLLQL.apply
}
