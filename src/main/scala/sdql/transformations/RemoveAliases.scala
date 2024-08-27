package sdql.transformations

import sdql.ir.*

private object RemoveAliases extends TermRewriter {
  private type AliasCtx = Map[Sym, Sym]

  def apply(e: Exp): Exp = run(e)

  private def run(e: Exp)(implicit aliasCtx: AliasCtx = Map()): Exp = e match {
    case LetBinding(x, e1: Sym, e2) =>
      val ctx = aliasCtx.get(e1) match {
        case Some(r) => Map(x -> r)
        case None    => Map(x -> e1)
      }
      run(e2)(aliasCtx ++ ctx)

    case e: Sym =>
      aliasCtx.get(e) match {
        case Some(r) => r
        case None    => e
      }

    case _ => e.mapInner(run)
  }
}
