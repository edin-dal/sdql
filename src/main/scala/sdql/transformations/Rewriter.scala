package sdql.transformations

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.{ raise, Field }

import scala.PartialFunction.cond
import scala.annotation.tailrec

trait Transformation {
  def apply(e: Exp): Exp
  def ++(transformation: Transformation): TermRewriter = TermRewriter(this, transformation)
}

class TermRewriter(transformations: Transformation*) extends Transformation {
  def apply(e: Exp): Exp = transformations.foldLeft(e)((acc, f) => f.apply(acc))
}

object TermRewriter { def apply(transformations: Transformation*): TermRewriter = new TermRewriter(transformations*) }

/** Applies all transformations and lowers an expression to LLQL */
object Rewriter {
  val rewrite: TermRewriter =
    RemoveAliases ++
      RemoveRecordGet ++
      SkipUnusedColumns ++
      RemoveIntermediateTuples ++
      BindFreeExpression ++
      LowerToLLQL

  def mapInner(f: Exp => Exp)(e: Exp): Exp = e match {
    case Restage(cs, fact) => fact(cs.map(f))
  }

  def mapInnerReduce[T](f: Exp => T, g: (T, T) => T, default: T)(e: Exp): T = e match {
    case Restage(cs, _) =>
      val fcs = cs.map(f)
      if (fcs.isEmpty)
        default
      else
        fcs.reduceLeft(g)
  }
}

/** Removes variable aliases, to enable further optimisations – see tests for examples. */
private object RemoveAliases extends Transformation {
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

/** Accesses record fields by name rather than by index, to enable further optimisations – see tests for examples. */
private object RemoveRecordGet extends Transformation {
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

/** Speeds up CSV loads by skipping columns that aren't used anywhere in the program – see tests for examples. */
private object SkipUnusedColumns extends Transformation {
  private type Columns = Map[Sym, Set[Field]]

  def apply(e: Exp): Exp = run(e)(find(e))

  private def run(e: Exp)(implicit columnsCtx: Columns): Exp = e match {
    case LetBinding(x, Load(path, tp: RecordType, skipCols), e2) =>
      val old  = skipCols.toSkipColsSet
      val new_ = tp.attrs.map(_.name: String).filter(!columnsCtx.getOrElse(x, Set()).contains(_)).toSet
      val skip = SetNode.fromSkipColsSet(old | new_)
      LetBinding(x, Load(path, tp, skip), run(e2))
    case _                                                       => Rewriter.mapInner(run)(e)
  }

  private def find(e: Exp)(implicit columnsCtx: Columns = Map()): Columns = e match {
    case LetBinding(x, _: Load, e2)                     => find(e2)(sumColumns(columnsCtx, Map(x -> Set())))
    case FieldNode(e: Sym, f) if columnsCtx.contains(e) => sumColumns(columnsCtx, Map(e -> Set(f)))
    case _                                              => Rewriter.mapInnerReduce[Columns](find, sumColumns, Map())(e)
  }

  private def sumColumns(a: Columns, b: Columns) =
    a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set())) }
}

/** Removes intermediate tuples, usually created after iterating on a range – see tests for examples. */
private object RemoveIntermediateTuples extends Transformation {
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

/** Binds to a variable the free expression at the end of a program, to aid code generation – see tests for examples. */
private object BindFreeExpression extends Transformation {
  def apply(e: Exp): Exp = e match {
    case DictNode(Nil, _)      => e // just in case of repeated applications
    case LetBinding(x, e1, e2) => LetBinding(x, e1, apply(e2))
    case e                     => LetBinding(Sym(resultName), e, DictNode(Nil))
  }
}

/** Lowers a SDQL expression to LLQL by introducing control-flow nodes to model "sum" – see tests for examples. */
private object LowerToLLQL extends Transformation {
  private type TypesCtx = TypeInference.Ctx

  def apply(e: Exp): Exp = run(e)(Map(), dest = None)

  private def run(e: Exp)(implicit ctx: TypesCtx, dest: Option[Sym]): Exp = e match {
    case LetBinding(x, e1: Sum, e2) => run(LetBinding(x, sumToInitialise(e1)(ctx, Some(x)), e2))
    case LetBinding(x, e1, e2)      => LetBinding(x, e1, run(e2)(ctx ++ Map(x -> TypeInference.run(e1)), dest))
    case IfThenElse(cond, e1, e2)   => IfThenElse(cond, run(e1), run(e2))
    case Sum(key, value, e1, e2)    =>
      val (_, ctxLocal) = TypeInference.sumInferTypeAndCtx(key, value, e1, e2)
      Sum(key, value, run(e1)(ctx, None), run(e2)(ctxLocal, dest))
    case _                          =>
      dest match {
        case Some(dest) => sumBodyToLLQL(e, dest)(ctx)
        case None       => Rewriter.mapInner(run)(e)
      }
  }

  private def sumBodyToLLQL(e: Exp, dest: Sym)(implicit ctx: TypesCtx) =
    if (isUpdate(e)) Update(e, Aggregation.fromExpression(e), dest) else Modify(e, dest)

  private def isUpdate(e: Exp)(implicit ctx: TypesCtx) = sumHint(e) match {
    case Some(_: PHmap) if cond(e) { case dict: DictNode => checkIsUnique(dict) } => false
    case None | Some(_: PHmap | _: SmallVecDict | _: SmallVecDicts)               => true
    case Some(_: Vec)                                                             => false
  }

  private def sumHint(e: Exp)(implicit ctx: TypesCtx) = e match {
    case dict @ DictNode(map, _) if map.nonEmpty =>
      (TypeInference.run(dict.getInnerDict): @unchecked) match { case DictType(_, _, hint) => Some(hint) }
    case _                                       => None
  }

  private def checkIsUnique(dict: DictNode) = cond(dict.getInnerDict) { case DictNode(Seq((_: Unique, _)), _: PHmap) =>
    true
  }

  private def sumToInitialise(e: Sum)(implicit ctx: TypesCtx, dest: Option[Sym]) =
    e match {
      case Sum(k, v, e1, e2) =>
        val (tpe, ctxLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
        val packed          = getSumBody(e2) match {
          case Promote(tsrt: TropicalSemiRingType, _) => TropicalSemiRingType.pack(tsrt, tpe)
          case _                                      => tpe
        }
        Initialise(packed, Sum(k, v, e1, run(e2)(ctxLocal, dest)))
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
