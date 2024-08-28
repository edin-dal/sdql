package sdql.transformations

import sdql.ir.*
import sdql.raise

private trait TermRewriter { def apply(e: Exp): Exp }

object Rewriter {
  private val rewriters = Seq(
    RemoveAliases,
    RemoveRecordGet,
    SkipUnusedColumns,
    RemoveIntermediateTuples,
    BindFreeExpression,
    LowerToLLQL,
  )

  def apply(e: Exp): Exp = rewriters.foldLeft(e) { (acc, f) =>
    f(acc)
  }

  def mapInner(f: Exp => Exp)(e: Exp): Exp = e match {
    // 0-ary
    case _: Sym | _: Const | _: Load => e
    // 1-ary
    case Neg(e)              => Neg(f(e))
    case FieldNode(e, field) => FieldNode(f(e), field)
    case Promote(tp, e)      => Promote(tp, f(e))
    case RangeNode(e)        => RangeNode(f(e))
    case Unique(e)           => Unique(f(e))
    // 2-ary
    case Add(e1, e2)             => Add(f(e1), f(e2))
    case Mult(e1, e2)            => Mult(f(e1), f(e2))
    case Cmp(e1, e2, cmp)        => Cmp(f(e1), f(e2), cmp)
    case Sum(key, value, e1, e2) => Sum(key, value, f(e1), f(e2))
    case Get(e1, e2)             => Get(f(e1), f(e2))
    case Concat(e1, e2)          => Concat(f(e1), f(e2))
    case LetBinding(x, e1, e2)   => LetBinding(x, f(e1), f(e2))
    // 3-ary
    case IfThenElse(e1, e2, e3) => IfThenElse(f(e1), f(e2), f(e3))
    // n-ary
    case RecNode(values) => RecNode(values.map(v => (v._1, f(v._2))))
    case DictNode(map, hint) =>
      DictNode(map.map(x => (f(x._1), f(x._2))), hint match {
        case PHmap(Some(e)) => PHmap(Some(f(e)))
        case _              => hint
      })
    case External(name, args) => External(name, args.map(f))
    case _                    => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }

  def mapInnerReduce[T](f: Exp => T, g: (T, T) => T, default: T)(e: Exp): T = e match {
    // 0-ary
    case _: Sym | _: Const | _: Load => default
    // 1-ary
    case Neg(e)          => f(e)
    case FieldNode(e, _) => f(e)
    case Promote(_, e)   => f(e)
    case RangeNode(e)    => f(e)
    case Unique(e)       => f(e)
    // 2-ary
    case Add(e1, e2)           => g(f(e1), f(e2))
    case Mult(e1, e2)          => g(f(e1), f(e2))
    case Cmp(e1, e2, _)        => g(f(e1), f(e2))
    case Sum(_, _, e1, e2)     => g(f(e1), f(e2))
    case Get(e1, e2)           => g(f(e1), f(e2))
    case Concat(e1, e2)        => g(f(e1), f(e2))
    case LetBinding(_, e1, e2) => g(f(e1), f(e2))
    // 3-ary
    case IfThenElse(e1, e2, e3) => g(g(f(e1), f(e2)), f(e3))
    // n-ary
    case RecNode(values) => values.map(_._2).map(f).foldLeft(default)(g)
    case DictNode(map, hint) =>
      g(g(map.map(_._1).map(f).foldLeft(default)(g), map.map(_._2).map(f).foldLeft(default)(g)), hint match {
        case PHmap(Some(e)) => f(e)
        case _              => default
      })
    case External(_, args) => args.map(f).foldLeft(default)(g)
    case _                 => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }
}
