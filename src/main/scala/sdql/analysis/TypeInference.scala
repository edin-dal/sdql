package sdql
package analysis

import munit.Assertions.munitPrint
import sdql.ir.ExternalFunctions._
import sdql.ir._

import scala.annotation.tailrec

object TypeInference {
  type Type = ir.Type
  type Var = Sym
  type Ctx = Map[Var, Type]

  def apply(e: Exp): Type = run(e)(Map())

  @tailrec
  def run(e: Exp)(implicit ctx: Ctx): Type = e match {
    case e: Sum => run(e)
    case e: IfThenElse => run(e)
    case e: Add => run(e)
    case e: Mult => run(e)
    case e: Neg => run(e)
    case e: Sym => run(e)
    case e: DictNode => run(e)
    case e: RecNode => run(e)
    case e: Cmp => run(e)
    case e: FieldNode => run(e)
    case e: Const => run(e)
    case e: Get => run(e)
    case e: External => run(e)
    case e: LetBinding => run(e)
    case e: Load => run(e)
    case e: Concat => run(e)

    case Unique(e: Exp) => run(e)
    case Promote(_, e: Exp) => run(e)

    case _ => raise(
      f"""Unhandled ${e.getClass.toString} in
         |${munitPrint(e)}""".stripMargin
    )
  }

  def run(e: IfThenElse)(implicit ctx: Ctx): Type = e match {
    case IfThenElse(a, Const(false), Const(true)) => run(a)
    case IfThenElse(_, DictNode(Nil, _), DictNode(Nil, _)) => raise("both branches empty")
    case IfThenElse(_, DictNode(Nil, _), e2) => run(e2)
    case IfThenElse(_, e1, DictNode(Nil, _)) => run(e1)
    case _: IfThenElse => branching(e)
  }

  def run(e: Sum)(implicit ctx: Ctx): Type = e match { case Sum(k, v, e1, e2) => sunInferTypeAndCtx(k, v, e1, e2)._1 }

  def run(e: Add)(implicit ctx: Ctx): Type = branching(e)

  def run(e: Mult)(implicit ctx: Ctx): Type = branching(e)

  def run(e: Neg)(implicit ctx: Ctx): Type = e match { case Neg(e) => run(e) }

  def run(e: Sym)(implicit ctx: Ctx): Type = e match {
    case sym @ Sym(name) =>
      ctx.get(sym) match {
        case Some(tpe) => tpe
        case None => raise(s"unknown name: $name")
      }
  }

  def run(e: DictNode)(implicit ctx: Ctx): Type = e match {
    case DictNode(Nil, _) =>
      raise("Type inference needs backtracking to infer empty type { }")
    case DictNode(seq, hint) =>
      DictType(seq.map(_._1).map(run).reduce(promote), seq.map(_._2).map(run).reduce(promote), hint)
  }

  def run(e: RecNode)(implicit ctx: Ctx): Type = e match {
    case RecNode(values) => RecordType(values.map(v => Attribute(name=v._1, tpe=run(v._2))))
  }

  def run(e: Cmp): Type = BoolType

  def run(e: FieldNode)(implicit ctx: Ctx): Type = e match {
    case FieldNode(e1: Exp, field) => run(e1) match {
      case tpe: RecordType => inferRecordField(tpe, field)
      case DictType(tpe: RecordType, IntType, _) => inferRecordField(tpe, field)
      case tpe => raise(s"unexpected type: ${tpe.prettyPrint}")
    }
  }

  private def inferRecordField(tpe: RecordType, field: Field) = {
    val attrs = tpe match { case RecordType(attrs) => attrs }
    tpe.indexOf(field) match {
      case Some(idx) => attrs(idx).tpe
      case None => raise(attrs.map(_.name).mkString(s"$field not in: ", ", ", "."))
    }
  }

  def run(e: Const): Type = e match {
    case Const(v) => v match {
      case _: DateValue => DateType
      case _: Boolean => BoolType
      case _: Integer => IntType
      case _: Double => RealType
      case _: String => StringType()
      case v => raise(s"unhandled class: ${v.getClass.getSimpleName}")
    }
  }

  def run(e: Get)(implicit ctx: Ctx): Type = e match {
    case Get(e1, e2) => run(e1) match {
      case RecordType(attrs) => run(e2) match {
        case IntType => e2 match {
          case Const(v: Int) => attrs(v).tpe
          case tpe => raise(
            s"expected ${Const.getClass.getSimpleName.init}, not ${tpe.simpleName}"
          )
        }
        case tpe => raise(
          s"expected ${IntType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
        )
      }
      case DictType(kType, vType, _) => run(e2) match {
        case tpe if tpe == kType => vType
        case tpe => raise(
          s"can't index with ${tpe.simpleName} from ${DictType.getClass.getSimpleName.init}"
        )
      }
      case tpe => raise(
        s"expected ${RecordType.getClass.getSimpleName.init} or " +
          s"${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
      )
    }
  }

  def run(e: External)(implicit ctx: Ctx): Type = e match {
    case External(ConstantString.SYMBOL, args) =>
      val (str, maxLen) = args match { case Seq(Const(str: String), Const(maxLen: Int)) => (str, maxLen)}
      assert(maxLen == str.length + 1)
      StringType(Some(str.length))
    case External(StrContains.SYMBOL | StrStartsWith.SYMBOL | StrEndsWith.SYMBOL | StrContainsN.SYMBOL, _) =>
      BoolType
    case External(SubString.SYMBOL, args) =>
      val (str, start, end) = args match { case Seq(str, Const(start: Int), Const(end: Int)) => (str, start, end)}
      TypeInference.run(str) match {
        case StringType(None) => StringType(None)
        case StringType(Some(_)) => StringType(Some(end - start))
      }
    case External(StrIndexOf.SYMBOL | FirstIndex.SYMBOL | LastIndex.SYMBOL | Year.SYMBOL, _) =>
      IntType
    case External(ParseDate.SYMBOL, _) =>
      DateType
    case External(Inv.SYMBOL, args) =>
      val arg = args match { case Seq(e) => e }
      run(arg)
    case External(MaxValue.SYMBOL | Size.SYMBOL, args) =>
      val arg = args match { case Seq(e) => e }
      run(arg) match {
        case DictType(_, vt, _) => vt
        case tpe => raise(
          s"${MaxValue.SYMBOL} or ${Size.SYMBOL} expect arg " +
            s"${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
        )
      }
    case External(name @ Limit.SYMBOL, args) =>
      val arg = args match { case Seq(e, _, _) => e}
      run(arg) match {
        case tpe: DictType => tpe
        case tpe => raise(
          s"$name expects arg ${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
        )
      }
    case External(TopN.SYMBOL, _) =>
      raise(s"unimplemented function name: ${TopN.SYMBOL}")
    case External(CStore.SYMBOL, _) =>
      raise(s"unimplemented function name: ${CStore.SYMBOL}")
    case External(Log.SYMBOL, _) =>
      raise(s"unimplemented function name: ${Log.SYMBOL}")
    case External(name, _) =>
      raise(s"unknown function name: $name")
  }

  def run(e: Load): Type = e match {
    case Load(_, DictType(kt, vt, DictNoHint())) => DictType(kt, vt, DictLoadHint())
    case Load(_, tp) => raise(s"unexpected: ${tp.prettyPrint}")
  }

  def run(e: LetBinding)(implicit ctx: Ctx): Type = e match {
    case LetBinding(x, e1, e2) =>
      val t1 = TypeInference.run(e1)
      TypeInference.run(e2)(ctx ++ Map(x -> t1))
  }

  def run(e: Concat)(implicit ctx: Ctx): Type = e match {
    case Concat(e1, e2) => (run(e1), run(e2)) match {
      case (v1 @ RecordType(fs1), v2 @ RecordType(fs2)) =>
        val (fs1m, fs2m) = fs1.map(x1 => (x1.name, x1.tpe)).toMap -> fs2.map(x2 => (x2.name, x2.tpe)).toMap
        val common =
          fs1.filter(x1 => fs2m.contains(x1.name)).map(x1 => (x1.name, x1.tpe, fs2m(x1.name)))
        if(common.isEmpty)
          RecordType(fs1 ++ fs2)
        else
          if(common.forall(x => x._2 == x._3))
            RecordType(fs1 ++ fs2.filter(x2 => !fs1m.contains(x2.name)))
          else
            raise(s"`concat($v1, $v2)` with different values for the same field name")
      case (v1, v2) =>
        raise(s"`concat($v1,$v2)` needs records, but given `${v1.getClass}`, `${v2.getClass}`")
    }
  }

  private def branching(e: Exp)(implicit ctx: Ctx): Type = {
    val (e1, e2) = e match {
      // and case
      case IfThenElse(a, b, Const(false)) =>
        (a, b)
      // or case
      case IfThenElse(a, Const(true), b) =>
        (a, b)
      case IfThenElse(cond, e1, e2) =>
        assert(run(cond) == BoolType)
        (e1, e2)
      case Add(e1, e2) => (e1, e2)
      case Mult(e1, e2) => (e1, e2)
      case _ => raise(s"unhandled class: ${e.simpleName}")
    }
    val t1 = run(e1)
    val t2 = run(e2)
    promote(t1, t2)
  }

  private def promote(t1: Type, t2: Type): Type = {
    (t1, t2) match {
      case (IntType, DateType) | (DateType, IntType) =>
        IntType
      case (IntType, RealType) | (RealType, IntType) =>
        RealType
      case (DictType(kt1, vt1, hint1), DictType(kt2, vt2, hint2)) =>
        assert(hint1 == hint2)
        DictType(promote(kt1, kt2), promote(vt1, vt2))
      case (DictType(kt, vt, hint), t) if t.isScalar =>
        DictType(kt, promote(vt, t), hint)
      case (t, DictType(kt, vt, hint)) if t.isScalar =>
        DictType(kt, promote(vt, t), hint)
      case (t1, t2) if t1 == t2 =>
        t1
      case (t1, t2) =>
        raise(s"can't promote types: ${t1.simpleName} ≠ ${t2.simpleName}")
    }
  }

  def sunInferTypeAndCtx(k: Sym, v: Sym, e1: Exp, e2: Exp)(implicit ctx: Ctx): (Type, Ctx) = {
    // from e1 infer types of k, v
    val (kType, vType) = run(e1) match {
      case DictType(k_type, v_type, _) => (k_type, v_type)
      case tpe => raise(
        s"assignment should be from ${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
      )
    }
    // from types of k, v infer type of e2
    val localCtx = ctx ++ Map(k -> kType, v -> vType)
    val tpe = run(e2)(localCtx)
    (tpe, localCtx)
  }
}
