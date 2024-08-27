package sdql.transformations

import sdql.ir.*

private object RemoveRecordGet extends TermRewriter {
  private type Names = Map[Sym, Seq[String]]

  def apply(e: Exp): Exp = run(e)(find(e))

  private def run(e: Exp)(implicit columnsCtx: Names): Exp = e match {
    case Get(e1: Sym, Const(v: Int))
        if columnsCtx.contains(e1) && columnsCtx(e1).diff(columnsCtx(e1).distinct).isEmpty =>
      FieldNode(e1, columnsCtx(e1)(v - 1)) // SDQL is 1-indexed
    case _ => e.mapInner(run)
  }

  private def find(e: Exp)(implicit names: Names = Map()): Names = e match {
    case LetBinding(x, e1: RecNode, e2)                => find(e2)(names ++ Map(x -> e1.values.map(_._1: String)))
    case LetBinding(x, Load(_, tp: RecordType, _), e2) => find(e2)(names ++ Map(x -> tp.attrs.map(_.name)))
    case _                                             => e.mapReduce[Names](find, (x, y) => x ++ y, names)
  }
}
