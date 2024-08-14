package sdql
package backend

import sdql.ir.*

import scala.PartialFunction.cond
import scala.annotation.tailrec

object Rewriter {
  private val rewriters = Seq(
    RemoveAliases,
//    RemoveRecordGet,
    SkipUnusedColumns,
    RemoveIntermediateTuples
  )

  def apply(e: Exp): Exp = rewriters.foldLeft(e) { (acc, f) =>
    f(acc)
  }
}

private trait TermRewriter { def apply(e: Exp): Exp }

private object RemoveRecordGet extends TermRewriter {
  def apply(e: Exp): Exp = ???
}

private object RemoveAliases extends TermRewriter {
  private type AliasCtx = Map[Sym, Sym]

  def apply(e: Exp): Exp = run(e)

  @tailrec
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

    case _ => runInner(e)
  }

  private def runInner(e: Exp)(implicit aliasCtx: AliasCtx) = e match {
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
    case External(name, args) => External(name, args.map(run(_)))
  }
}

private object SkipUnusedColumns extends TermRewriter {
  private type Columns = Map[Sym, Set[Field]]

  def apply(e: Exp): Exp = run(e)(find(e))

  private def run(e: Exp)(implicit columnsCtx: Columns): Exp = e match {
    case LetBinding(x, Load(path, tp: RecordType, skipCols), e2) =>
      val old  = skipCols.toSkipColsSet
      val new_ = tp.attrs.map(_.name: String).filter(!columnsCtx.getOrElse(x, Set()).contains(_)).toSet
      val skip = SetNode.fromSkipColsSet(old | new_)
      LetBinding(x, Load(path, tp, skip), run(e2))
    case _ => runInner(e)
  }
  private def runInner(e: Exp)(implicit columnsCtx: Columns) = e match {
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
    case External(name, args) => External(name, args.map(run(_)))
  }

  @tailrec
  private def find(e: Exp)(implicit columnsCtx: Columns = Map()): Columns = e match {
    case LetBinding(x, _: Load, e2)                           => find(e2)(sumColumns(columnsCtx, Map(x -> Set())))
    case FieldNode(e: Sym, f) if columnsCtx.contains(e)       => sumColumns(columnsCtx, Map(e -> Set(f)))
    case Get(e: Sym, Const(_: Int)) if columnsCtx.contains(e) => ??? // TODO pass around record names
    case _                                                    => findInner(e)
  }
  private def findInner(e: Exp)(implicit columnsCtx: Columns): Columns = e match {
    // 0-ary
    case _: Sym | _: Const | _: Load => Map()
    // 1-ary
    case Neg(e)          => find(e)
    case FieldNode(e, _) => find(e)
    case Promote(_, e)   => find(e)
    case RangeNode(e)    => find(e)
    case Unique(e)       => find(e)
    // 2-ary
    case Add(e1, e2)           => sumColumns(find(e1), find(e2))
    case Mult(e1, e2)          => sumColumns(find(e1), find(e2))
    case Cmp(e1, e2, _)        => sumColumns(find(e1), find(e2))
    case Sum(_, _, e1, e2)     => sumColumns(find(e1), find(e2))
    case Get(e1, e2)           => sumColumns(find(e1), find(e2))
    case Concat(e1, e2)        => sumColumns(find(e1), find(e2))
    case LetBinding(_, e1, e2) => sumColumns(find(e1), find(e2))
    // 3-ary
    case IfThenElse(e1, e2, e3) => sumColumns(sumColumns(find(e1), find(e2)), find(e3))
    // n-ary
    case RecNode(values)   => sumColumns(values.map(_._2).map(find))
    case DictNode(map, _)  => sumColumns((map.map(_._1) ++ map.map(_._2)).map(find))
    case External(_, args) => sumColumns(args.map(find))
  }
  private def sumColumns(cols: Iterable[Columns]): Columns = cols.foldLeft[Columns](Map()) { (acc, cols) =>
    sumColumns(acc, cols)
  }
  private def sumColumns(a: Columns, b: Columns) = a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set())) }
}

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
    case RecNode(values)      => RecNode(values.map(v => (v._1, run(v._2))))
    case DictNode(map, hint)  => DictNode(map.map(x => (run(x._1), run(x._2))), hint)
    case External(name, args) => External(name, args.map(run(_)))
  }
}
