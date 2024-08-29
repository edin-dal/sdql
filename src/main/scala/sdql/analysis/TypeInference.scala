package sdql
package analysis

import sdql.ir.*
import sdql.ir.ExternalFunctions.*

import scala.PartialFunction.cond

object TypeInference {
  type Type = ir.Type
  type Var  = Sym
  type Ctx  = Map[Var, Type]

  def apply(e: Exp): Type = run(e)(Map())

  def run(e: Exp)(implicit ctx: Ctx): Type = e match {
    case Sum(k, v, e1, e2) => sumInferTypeAndCtx(k, v, e1, e2)._1

    case IfThenElse(a, Const(false), Const(true)) => run(a)
    case IfThenElse(_,
                    DictNode(Nil, _) | Update(DictNode(Nil, _), _, _),
                    DictNode(Nil, _) | Update(DictNode(Nil, _), _, _)) =>
      raise("both branches empty")
    case IfThenElse(_, DictNode(Nil, _) | Update(DictNode(Nil, _), _, _), e2) => run(e2)
    case IfThenElse(_, e1, DictNode(Nil, _) | Update(DictNode(Nil, _), _, _)) => run(e1)
    case _: IfThenElse                                                        => branching(e)

    case _: Add => branching(e)

    case _: Mult => branching(e)

    case Neg(e) => run(e)

    case sym @ Sym(name) =>
      ctx.get(sym) match {
        case Some(tpe) => tpe
        case None      => raise(s"unknown name: $name")
      }

    case DictNode(Nil, _) => raise("Type inference needs backtracking to infer empty type { }")
    case DictNode(seq, hint) =>
      DictType(seq.map(_._1).map(run).reduce(promote), seq.map(_._2).map(run).reduce(promote), hint)

    case RecNode(values) => RecordType(values.map(v => Attribute(name = v._1, tpe = run(v._2))))

    case _: Cmp => BoolType

    case FieldNode(e1: Exp, field) =>
      run(e1) match {
        case rt @ RecordType(attrs) =>
          rt.indexOf(field) match {
            case Some(idx) => attrs(idx).tpe
            case None      => raise(attrs.map(_.name).mkString(s"$field not in: ", ", ", "."))
          }
        case tpe => raise(s"unexpected type: ${tpe.prettyPrint} in\n${e.prettyPrint}")
      }

    case Const(v) =>
      v match {
        case _: DateValue => DateType
        case _: Boolean   => BoolType
        case _: Integer   => IntType
        case _: Double    => RealType
        case _: String    => StringType()
        case v            => raise(s"unhandled class: ${v.getClass.getSimpleName}")
      }

    case Get(e1, e2) =>
      run(e1) match {
        case RecordType(attrs) =>
          run(e2) match {
            case IntType =>
              e2 match {
                case Const(v: Int) => attrs(v).tpe
                case tpe =>
                  raise(s"expected ${Const.getClass.getSimpleName.init}, not ${tpe.simpleName}")
              }
            case tpe => raise(s"expected ${IntType.getClass.getSimpleName.init}, not ${tpe.simpleName}")
          }
        case DictType(kType, vType, _) =>
          run(e2) match {
            case tpe if tpe == kType => vType
            case tpe =>
              raise(s"can't index with ${tpe.simpleName} from ${DictType.getClass.getSimpleName.init}")
          }
        case tpe =>
          raise(
            s"expected ${RecordType.getClass.getSimpleName.init} or " +
              s"${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
          )
      }

    case External(ConstantString.SYMBOL, args) =>
      val (str, maxLen) = args match { case Seq(Const(str: String), Const(maxLen: Int)) => (str, maxLen) }
      assert(maxLen == str.length + 1)
      StringType(Some(str.length))
    case External(StrContains.SYMBOL | StrStartsWith.SYMBOL | StrEndsWith.SYMBOL | StrContainsN.SYMBOL, _) => BoolType
    case External(SubString.SYMBOL, args) =>
      val (str, start, end) = args match { case Seq(str, Const(start: Int), Const(end: Int)) => (str, start, end) }
      TypeInference.run(str) match {
        case StringType(None)    => StringType(None)
        case StringType(Some(_)) => StringType(Some(end - start))
        case t                   => raise(s"unexpected: ${t.prettyPrint}")
      }
    case External(StrIndexOf.SYMBOL | FirstIndex.SYMBOL | LastIndex.SYMBOL | Year.SYMBOL, _) => IntType
    case External(ParseDate.SYMBOL, _)                                                       => DateType
    case External(Inv.SYMBOL, args) =>
      val arg = args match { case Seq(e) => e }
      run(arg)
    case External(name @ Size.SYMBOL, args) =>
      val arg = args match { case Seq(e) => e }
      run(arg) match {
        case DictType(_, vt, _) => vt
        case tpe =>
          raise(s"$name expect arg ${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}")
      }
    case External(TopN.SYMBOL, _)   => raise(s"unimplemented function name: ${TopN.SYMBOL}")
    case External(CStore.SYMBOL, _) => raise(s"unimplemented function name: ${CStore.SYMBOL}")
    case External(Log.SYMBOL, _)    => raise(s"unimplemented function name: ${Log.SYMBOL}")
    case External(name, _)          => raise(s"unknown function name: $name")

    case LetBinding(Sym(name), e1, DictNode(Nil, _)) if name == resultName => run(e1)
    case LetBinding(x, e1, e2) =>
      val t1 = TypeInference.run(e1)
      TypeInference.run(e2)(ctx ++ Map(x -> t1))

    case e: Load =>
      e match {
        case Load(_, rt: RecordType, skipCols) if isColumnStore(rt) && skipCols.isSetNode =>
          val set = skipCols.toSkipColsSet
          RecordType(rt.attrs.filter(attr => !set.contains(attr.name)))
        case Load(_, tp, _) => raise(s"unexpected: ${tp.prettyPrint}")
      }

    case Concat(e1, e2) =>
      (run(e1), run(e2)) match {
        case (t1: RecordType, t2: RecordType) => t1.concat(t2)
        case (v1, v2) =>
          raise(s"`concat($v1,$v2)` needs records, but given `${v1.prettyPrint}`, `${v2.prettyPrint}`")
      }

    case Promote(_: TropicalSemiRingType, e) => run(e)
    case Promote(tp, _)                      => tp

    case Unique(e: Exp) => run(e)

    // LLQL
    case Initialise(tpe, _) => TropicalSemiRingType.unpack(tpe)
    case Update(e, _, _)    => run(e)
    case Modify(e, _)       => run(e)

    case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
  }

  def sumInferTypeAndCtx(k: Sym, v: Sym, e1: Exp, e2: Exp)(implicit ctx: Ctx): (Type, Ctx) = {
    val localCtx = ctx ++ (e1 match {
      case _: RangeNode => Map(k -> IntType)
      // from e1 infer types of k, v
      case _ =>
        run(e1) match {
          case DictType(kType, vType, _) => Map(k -> kType, v -> vType)
          case tpe =>
            raise(
              s"assignment should be from ${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
            )
        }
    })
    // from types of k, v infer type of e2
    val tpe = run(e2)(localCtx)
    (tpe, localCtx)
  }

  def isColumnStore(rt: RecordType): Boolean = {
    rt("size") match {
      case Some(IntType) =>
      case _             => return false
    }
    rt match {
      case RecordType(attrs) =>
        attrs.filter(_.name != "size").map(_.tpe).forall(cond(_) { case DictType(IntType, _, _) => true })
    }
  }

  private def branching(e: Exp)(implicit ctx: Ctx): Type = {
    val (e1, e2) = e match {
      case IfThenElse(a, b, Const(false)) => (a, b) // and case
      case IfThenElse(a, Const(true), b)  => (a, b) // or case
      case IfThenElse(cond, e1, e2) =>
        assert(run(cond) == BoolType)
        (e1, e2)
      case Add(e1, e2)  => (e1, e2)
      case Mult(e1, e2) => (e1, e2)
      case _            => raise(s"unhandled class: ${e.simpleName}")
    }
    val t1 = run(e1)
    val t2 = run(e2)
    promote(t1, t2)
  }

  private def promote(t1: Type, t2: Type): Type =
    (t1, t2) match {
      case (IntType, DateType) | (DateType, IntType) => IntType
      case (IntType, RealType) | (RealType, IntType) => RealType
      case (DictType(kt1, vt1, hint1), DictType(kt2, vt2, hint2)) =>
        assert(hint1 == hint2)
        DictType(promote(kt1, kt2), promote(vt1, vt2))
      case (DictType(kt, vt, hint), t) if t.isScalar => DictType(kt, promote(vt, t), hint)
      case (t, DictType(kt, vt, hint)) if t.isScalar => DictType(kt, promote(vt, t), hint)
      case (t1, t2) if t1 == t2                      => t1
      case (t1, t2) =>
        raise(s"can't promote types: ${t1.simpleName} ≠ ${t2.simpleName}")
    }
}
