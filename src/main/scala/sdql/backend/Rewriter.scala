package sdql
package backend

import sdql.ir._

import scala.PartialFunction.cond

/**
 * Term rewriter that removes intermediate tuples after sums – see test cases for examples
 */
object Rewriter {
  private type ReplaceCtx = Map[Sym, RecNode]

  def apply(e: Exp)(implicit replaceCtx: ReplaceCtx = Map()): Exp = e match {

    case LetBinding(x, e1: RecNode, e2) if isSimpleRecord(e1) => this(e2)(replaceCtx ++ Map(x -> e1))

    case FieldNode(e1: Sym, field) if replaceCtx.contains(e1) =>
      (replaceCtx(e1)(field): @unchecked) match { case Some(exp) => exp }

    case _ => applyInner(e)
  }

  private def isSimpleRecord(record: RecNode) =
    record match { case RecNode(values) => values.map(_._2).forall(isSimpleField) }

  private def isSimpleField(e: Exp) = cond(e) { case Get(FieldNode(_: Sym, _), _: Sym) => true }

  private def applyInner(e: Exp)(implicit replaceCtx: ReplaceCtx) = e match {
    // 0-ary
    case _: Sym | _: Const | _: RangeNode | _: Load => e
    // 1-ary
    case Neg(e) => Neg(this(e))
    case FieldNode(e, f) => FieldNode(this(e), f)
    case Promote(tp, e) => Promote(tp, this(e))
    case Unique(e) => Unique(this(e))
    // 2-ary
    case Add(e1, e2) => Add(this(e1), this(e2))
    case Mult(e1, e2) => Mult(this(e1), this(e2))
    case Cmp(e1, e2, cmp) => Cmp(this(e1), this(e2), cmp)
    case Sum(key, value, e1, e2) => Sum(key, value, this(e1), this(e2))
    case Get(e1, e2) => Get(this(e1), this(e2))
    case Concat(e1, e2) => Concat(this(e1), this(e2))
    case LetBinding(x, e1, e2) => LetBinding(x, this(e1), this(e2))
    // 3-ary
    case IfThenElse(e1, e2, e3) => IfThenElse(this(e1), this(e2), this(e3))
    // n-ary
    case RecNode(values) => RecNode(values.map(v => (v._1, this(v._2))))
    case DictNode(map, hint) => DictNode(map.map(x => (this(x._1), this(x._2))), hint)
    case External(name, args) => External(name, args.map(this(_)))
    // unhandled
    case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }
}
