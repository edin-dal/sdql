package sdql.transformations

import sdql.ir.*
import sdql.{ raise, Field }

import scala.annotation.tailrec

private object SkipUnusedColumns extends TermRewriter {
  private type Columns = Map[Sym, Set[Field]]

  def apply(e: Exp): Exp = run(e)(find(e))

  private def run(e: Exp)(implicit columnsCtx: Columns): Exp = e match {
    case LetBinding(x, Load(path, tp: RecordType, skipCols), e2) =>
      val old  = skipCols.toSkipColsSet
      val new_ = tp.attrs.map(_.name: String).filter(!columnsCtx.getOrElse(x, Set()).contains(_)).toSet
      val skip = SetNode.fromSkipColsSet(old | new_)
      LetBinding(x, Load(path, tp, skip), run(e2))
    case _ => e.mapInner(run)
  }

  @tailrec
  private def find(e: Exp)(implicit columnsCtx: Columns = Map()): Columns = e match {
    case LetBinding(x, _: Load, e2)                     => find(e2)(sumColumns(columnsCtx, Map(x -> Set())))
    case FieldNode(e: Sym, f) if columnsCtx.contains(e) => sumColumns(columnsCtx, Map(e -> Set(f)))
    case _                                              => findInner(e)
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
    case RecNode(values)               => sumColumns(values.map(_._2).map(find))
    case DictNode(map, PHmap(Some(e))) => sumColumns((map.map(_._1) ++ map.map(_._2)).map(find) ++ Seq(find(e)))
    case DictNode(map, _)              => sumColumns((map.map(_._1) ++ map.map(_._2)).map(find))
    case External(_, args)             => sumColumns(args.map(find))
    case _                             => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }
  private def sumColumns(cols: Iterable[Columns]): Columns = cols.foldLeft[Columns](Map()) { (acc, cols) =>
    sumColumns(acc, cols)
  }
  private def sumColumns(a: Columns, b: Columns) = a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set())) }
}
