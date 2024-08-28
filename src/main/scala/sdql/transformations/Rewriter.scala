package sdql.transformations

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.{ raise, Field }

import scala.PartialFunction.cond
import scala.annotation.tailrec

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

    case _ => Rewriter.mapInner(run)(e)
  }
}

private object RemoveRecordGet extends TermRewriter {
  private type Names = Map[Sym, Seq[String]]

  def apply(e: Exp): Exp = run(e)(find(e))

  private def run(e: Exp)(implicit columnsCtx: Names): Exp = e match {
    case Get(e1: Sym, Const(v: Int))
        if columnsCtx.contains(e1) && columnsCtx(e1).diff(columnsCtx(e1).distinct).isEmpty =>
      FieldNode(e1, columnsCtx(e1)(v - 1)) // SDQL is 1-indexed
    case _ => Rewriter.mapInner(run)(e)
  }

  private def find(e: Exp)(implicit names: Names = Map()): Names = e match {
    case LetBinding(x, e1: RecNode, e2)                => find(e2)(names ++ Map(x -> e1.values.map(_._1: String)))
    case LetBinding(x, Load(_, tp: RecordType, _), e2) => find(e2)(names ++ Map(x -> tp.attrs.map(_.name)))
    case _                                             => Rewriter.mapInnerReduce[Names](find, (x, y) => x ++ y, names)(e)
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
    case _ => Rewriter.mapInner(run)(e)
  }

  private def find(e: Exp)(implicit columnsCtx: Columns = Map()): Columns = e match {
    case LetBinding(x, _: Load, e2)                     => find(e2)(sumColumns(columnsCtx, Map(x -> Set())))
    case FieldNode(e: Sym, f) if columnsCtx.contains(e) => sumColumns(columnsCtx, Map(e -> Set(f)))
    case _                                              => Rewriter.mapInnerReduce[Columns](find, sumColumns, Map())(e)
  }

  private def sumColumns(a: Columns, b: Columns) =
    a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set())) }
}

private object RemoveIntermediateTuples extends TermRewriter {
  private type ReplaceCtx = Map[Sym, RecNode]

  def apply(e: Exp): Exp = run(e)

  private def run(e: Exp)(implicit replaceCtx: ReplaceCtx = Map()): Exp = e match {
    case LetBinding(x, e1: RecNode, e2) if isSimpleRecord(e1) => run(e2)(replaceCtx ++ Map(x -> e1))

    case FieldNode(e1: Sym, field) if replaceCtx.contains(e1) =>
      (replaceCtx(e1)(field): @unchecked) match { case Some(exp) => exp }

    case e1: Sym if replaceCtx.contains(e1) => replaceCtx(e1)

    case _ => Rewriter.mapInner(run)(e)
  }

  private def isSimpleRecord(record: RecNode) =
    record match { case RecNode(values) => values.map(_._2).forall(isSimpleField) }

  private def isSimpleField(e: Exp) = cond(e) { case Get(FieldNode(_: Sym, _), _: Sym) => true }
}

private object BindFreeExpression extends TermRewriter {
  def apply(e: Exp): Exp = e match {
    case DictNode(Nil, _)      => e // just in case of repeated applications
    case LetBinding(x, e1, e2) => LetBinding(x, e1, apply(e2))
    case e                     => LetBinding(Sym(resultName), e, DictNode(Nil))
  }
}

private object LowerToLLQL extends TermRewriter {
  private type TypesCtx = TypeInference.Ctx

  def apply(e: Exp): Exp = run(e)(Map(), dest = None)

  private def run(e: Exp)(implicit ctx: TypesCtx, dest: Option[Sym]): Exp = e match {
    case LetBinding(x, e1: Sum, e2) => run(LetBinding(x, sumToInitialise(e1)(ctx, Some(x)), e2))
    case LetBinding(x, e1, e2)      => LetBinding(x, e1, run(e2)(ctx ++ Map(x -> TypeInference.run(e1)), dest))
    case IfThenElse(cond, e1, e2)   => IfThenElse(cond, run(e1), run(e2))
    case Sum(key, value, e1, e2) =>
      val (_, ctxLocal) = TypeInference.sumInferTypeAndCtx(key, value, e1, e2)
      Sum(key, value, run(e1)(ctx, None), run(e2)(ctxLocal, dest))
    case _ =>
      dest match {
        case Some(dest) => sumBodyToLLQL(e, dest)(ctx)
        case None       => Rewriter.mapInner(run)(e)
      }
  }

  private def sumBodyToLLQL(e: Exp, dest: Sym)(implicit ctx: TypesCtx) =
    if (isUpdate(e)) Update(e, Aggregation.fromExpression(e), dest) else Modify(e, dest)

  private def isUpdate(e: Exp)(implicit ctx: TypesCtx) = sumHint(e) match {
    case Some(_: PHmap) if cond(e) { case dict: DictNode => checkIsUnique(dict) } => false
    case None | Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts) => true
    case Some(_: Vec)                                               => false
  }

  private def sumHint(e: Exp)(implicit ctx: TypesCtx) = e match {
    case dict @ DictNode(map, _) if map.nonEmpty =>
      (TypeInference.run(dict.getInnerDict): @unchecked) match { case DictType(_, _, hint) => Some(hint) }
    case _ => None
  }

  private def checkIsUnique(dict: DictNode) = cond(dict.getInnerDict) {
    case DictNode(Seq((_: Unique, _)), _: PHmap) => true
  }

  private def sumToInitialise(e: Sum)(implicit ctx: TypesCtx, dest: Option[Sym]) =
    e match {
      case Sum(k, v, e1, e2) =>
        val (tpe, ctxLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
        val agg             = Aggregation.fromExpression(getSumBody(e2))
        Initialise(tpe, agg, Sum(k, v, e1, run(e2)(ctxLocal, dest)))
    }

  @tailrec
  private def getSumBody(e: Exp): Exp = e match {
    case Sum(_, _, _, e2)                                  => getSumBody(e2)
    case LetBinding(_, _, e2)                              => getSumBody(e2)
    case IfThenElse(_, DictNode(Nil, _), DictNode(Nil, _)) => raise("both branches empty")
    case IfThenElse(_, DictNode(Nil, _), e2)               => getSumBody(e2)
    case IfThenElse(_, e1, DictNode(Nil, _))               => getSumBody(e1)
    case IfThenElse(_, e1, _)                              => getSumBody(e1) // pick arbitrary branch
    case _                                                 => e
  }
}
