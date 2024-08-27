package sdql.transformations

import sdql.Field
import sdql.ir.*

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

  private def find(e: Exp)(implicit columnsCtx: Columns = Map()): Columns = e match {
    case LetBinding(x, _: Load, e2)                     => find(e2)(sumColumns(columnsCtx, Map(x -> Set())))
    case FieldNode(e: Sym, f) if columnsCtx.contains(e) => sumColumns(columnsCtx, Map(e -> Set(f)))
    case _                                              => e.mapReduce[Columns](find, sumColumns, () => Map())
  }

  private def sumColumns(a: Columns, b: Columns) = a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set())) }
}
