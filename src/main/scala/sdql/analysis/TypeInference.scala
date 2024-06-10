package sdql
package analysis

import munit.Assertions.munitPrint
import sdql.ir._

object TypeInference {
  type Type = ir.Type
  type Var = Sym
  type Ctx = Map[Var, Type]

  def apply(e: Exp): Type = run(e)(Map())

  def run(e: Exp)(implicit ctx: Ctx): Type = {
    e match {
      // iterating over a map has the same type whether we replace or sum values
      case _: ForLoop =>
        loop_infer_type_and_ctx(e)._1
      case _: Sum =>
        loop_infer_type_and_ctx(e)._1

      // not case
      case IfThenElse(a, Const(false), Const(true)) =>
        run(a)
      case IfThenElse(_, DictNode(Nil), DictNode(Nil)) =>
        raise("both branches empty")
      case IfThenElse(_, DictNode(Nil), e2) =>
        run(e2)
      case IfThenElse(_, e1, DictNode(Nil)) =>
        run(e1)
      case _: IfThenElse | _: Add | _: Mult =>
        branching(e)

      case Neg(e) =>
        run(e)

      case sym @ Sym(name) =>
        ctx.get(sym) match {
          case Some(tpe) => tpe
          case None => raise(s"unknown name: $name")
        }

      case DictNode(Seq((e1: RecNode, e2: RecNode))) =>
        DictType(run(e1), run(e2))
      case DictNode(Seq((e1, e2))) =>
        DictType(run(e1), run(e2))

      case RecNode(values) =>
        RecordType(values.map(v => Attribute(name=v._1, tpe=run(v._2))))

      case Cmp(_, _, _) =>
        BoolType

      case FieldNode(sym @ Sym(name), f) => run(sym) match {
        case RecordType(vals) => vals.find(_.name == f) match {
          case Some(v) => v.tpe
          case _ => raise(vals.map(_.name).mkString(s"$name not in: ", ", ", "."))
        }
        case t => raise(
          s"${Sym.getClass.getSimpleName.init} $name: expected " +
            s"${RecordType.getClass.getSimpleName.init}, not ${t.simpleName}"
        )
      }

      case Const(v) =>
        any(v)

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
        case DictType(kType, vType) => run(e2) match {
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

      case External(name, args) =>
        import ExternalFunctions._
        name match {
          case _ if name == StrContains.SYMBOL =>
            BoolType
          case _ if name == StrStartsWith.SYMBOL =>
            BoolType
          case _ if name == StrEndsWith.SYMBOL =>
            BoolType
          case _ if name == StrContainsN.SYMBOL =>
            BoolType
          case _ if name == SubString.SYMBOL =>
            StringType
          case _ if name == StrIndexOf.SYMBOL =>
            IntType
          case _ if name == Year.SYMBOL =>
            IntType
          case _ if name == ParseDate.SYMBOL =>
            DateType
          case _ if name == Inv.SYMBOL =>
            val arg = args match { case Seq(e) => e}
            run(arg)
          case _ if Set(TopN.SYMBOL, CStore.SYMBOL, Log.SYMBOL).contains(name) =>
            raise(s"unimplemented function name: $name")
          case _ =>
            raise(s"unknown function name: $name")
        }

      case LetBinding(x, e1, e2) =>
        val t1 = TypeInference.run(e1)
        TypeInference.run(e2)(ctx ++ Map(x -> t1))

      case Load(_, tp) =>
        tp

      case _ => raise(
        f"""Unhandled ${e.getClass.toString} in
           |${munitPrint(e)}""".stripMargin
      )
    }
  }

  def loop_infer_type_and_ctx(e: Exp)(implicit ctx: Ctx): (Type, Ctx) = {
    val (k, v, e1, e2) = unpack_loop(e)
    val e1Sym = e1 match { case e1Sym: Sym => e1Sym }
    // from e1 infer types of k, v
    val (kType, vType) = ctx.get(e1Sym) match {
      case Some(DictType(k_type, v_type)) => (k_type, v_type)
      case Some(tpe) => raise(
        s"assignment should be from ${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
      )
      case None => raise(s"unknown symbol: $e1Sym")
    }
    // from types of k, v infer type of e2
    val localCtx = ctx ++ Map(k -> kType, v -> vType)
    (run(e2)(localCtx), localCtx)
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
    val (t1Promo, t2Promo) = (t1, t2) match {
      case (IntType, RealType) | (RealType, IntType) => (RealType, RealType)
      case (t1, t2) => (t1, t2)
    }
    if (t1Promo != t2Promo) {
      raise(
        s"branching with types: " +
          s"${t1Promo.simpleName}${if (t1Promo != t1) s" (↑${t1.simpleName})" else ""} ≠ " +
          s"${t2Promo.simpleName}${if (t2Promo != t2) s" (↑${t2.simpleName})" else ""}"
      )
    }
    t1Promo
  }

  def unpack_loop(e: Exp): (Var, Var, Exp, Exp) = e match {
    case ForLoop(k, v, e1, e2) => (k, v, e1, e2)
    case Sum(k, v, e1, e2) => (k, v, e1, e2)
    case tpe => raise(
      s"expected ${ForLoop.getClass.getSimpleName.init} or " +
        s"${Sum.getClass.getSimpleName.init}, not ${tpe.simpleName}"
    )
  }

  private def any(v: Any): Type = v match {
    case _: DateValue => DateType
    case _: Boolean => BoolType
    case _: Integer => IntType
    case _: Double => RealType
    case _: String => StringType
    case v => raise(s"unhandled class: ${v.getClass.getSimpleName}")
  }
}
