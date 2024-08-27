package sdql.transformations

import sdql.ir.*
import sdql.raise

import scala.annotation.tailrec

private object RemoveRecordGet extends TermRewriter {
  private type Names = Map[Sym, Seq[String]]

  def apply(e: Exp): Exp = run(e)(find(e))

  private def run(e: Exp)(implicit columnsCtx: Names): Exp = e match {
    case Get(e1: Sym, Const(v: Int))
        if columnsCtx.contains(e1) && columnsCtx(e1).diff(columnsCtx(e1).distinct).isEmpty =>
      FieldNode(e1, columnsCtx(e1)(v - 1)) // SDQL is 1-indexed
    case _ => e.mapInner(run)
  }

  @tailrec
  private def find(e: Exp)(implicit names: Names = Map()): Names = e match {
    case LetBinding(x, e1: RecNode, e2)                => find(e2)(names ++ Map(x -> e1.values.map(_._1: String)))
    case LetBinding(x, Load(_, tp: RecordType, _), e2) => find(e2)(names ++ Map(x -> tp.attrs.map(_.name)))
    case _                                             => findInner(e)
  }
  private def findInner(e: Exp)(implicit names: Names): Names = e match {
    // 0-ary
    case _: Sym | _: Const | _: Load => names
    // 1-ary
    case Neg(e)          => find(e)
    case FieldNode(e, _) => find(e)
    case Promote(_, e)   => find(e)
    case RangeNode(e)    => find(e)
    case Unique(e)       => find(e)
    // 2-ary
    case Add(e1, e2)           => find(e1) ++ find(e2)
    case Mult(e1, e2)          => find(e1) ++ find(e2)
    case Cmp(e1, e2, _)        => find(e1) ++ find(e2)
    case Sum(_, _, e1, e2)     => find(e1) ++ find(e2)
    case Get(e1, e2)           => find(e1) ++ find(e2)
    case Concat(e1, e2)        => find(e1) ++ find(e2)
    case LetBinding(_, e1, e2) => find(e1) ++ find(e2)
    // 3-ary
    case IfThenElse(e1, e2, e3) => find(e1) ++ find(e2) ++ find(e3)
    // n-ary
    case RecNode(values)               => concat(values.map(_._2).map(find))
    case DictNode(map, PHmap(Some(e))) => concat((map.map(_._1) ++ map.map(_._2)).map(find)) ++ find(e)
    case DictNode(map, _)              => concat((map.map(_._1) ++ map.map(_._2)).map(find))
    case External(_, args)             => concat(args.map(find))
    case _                             => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }
  private def concat(cols: Iterable[Names]): Names = cols.flatten.toMap
}
