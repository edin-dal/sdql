package sdql.transformations

import sdql.ir.*

private object BindFreeExpression extends TermRewriter {
  def apply(e: Exp): Exp = e match {
    case DictNode(Nil, _)      => e // just in case of repeated applications
    case LetBinding(x, e1, e2) => LetBinding(x, e1, apply(e2))
    case e                     => LetBinding(Sym(resultName), e, DictNode(Nil))
  }
}
