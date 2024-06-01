package sdql
package backend

import munit.Assertions.munitPrint
import sdql.ir._

import scala.collection.mutable.ArrayBuffer

object TypeInference {
  type Type = ir.Type
  type Var = Sym
  type Ctx = Map[Var, Type]

  def apply(e: Exp): Type = run(e)(Map())

  def run(e: Exp)(implicit ctx: Ctx): Type = {
    e match {
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

      case DictNode(ArrayBuffer((RecNode(keys), RecNode(values)))) =>
        assert(keys.isInstanceOf[ArrayBuffer[(Field, Exp)]])
        assert(values.isInstanceOf[ArrayBuffer[(Field, Exp)]])
        DictType(
          TupleType(keys.map(_._2).map(run)),
          TupleType(values.map(_._2).map(run)),
        )

      case FieldNode(sym@Sym(name), f) => ctx.get(sym) match {
        case Some(RecordType(vals)) => vals.find(v => v.name == f) match {
          case Some(v) => v.tpe
          case _ => raise(s"$name not in $vals")
        }
        case _ => raise(s"unknown name: $name")
      }

      case Const(v) => v match {
        case _: DateValue => DateType
        case v if v.getClass == classOf[Boolean] => BoolType
        case v if v.getClass == classOf[Integer] => IntType
        case v if v.getClass == classOf[Double] => RealType
        case v if v.getClass == classOf[String] => StringType
        case v => raise(s"unhandled class: ${v.getClass.getSimpleName}")
      }

      case Load(_, tp) =>
        tp

      case _ => raise(
        f"""Unhandled ${e.getClass.toString} in
           |${munitPrint(e)}""".stripMargin
      )
    }
  }

  private def branching(exp: Exp)(implicit ctx: Ctx): Type = {
    val (e1, e2) = exp match {
      case IfThenElse(_, e1, e2) => (e1, e2)
      case Add(e1, e2) => (e1, e2)
      case Mult(e1, e2) => (e1, e2)
      case _ => raise(s"unhandled class: ${exp.simpleName}")
    }
    val t1 = run(e1)
    val t2 = run(e2)
    val (t1Promo, t2Promo) = (t1, t2) match {
      case (IntType, RealType) | (RealType, IntType) => (RealType, RealType)
      case (t1, t2) => (t1, t2)
    }
    if (t1Promo != t2Promo) {
      raise(
        s"${exp.simpleName} branches have types: " +
        s"${t1Promo.simpleName}${if (t1Promo != t1) s" (↑${t1.simpleName})" else ""} ≠ " +
        s"${t2Promo.simpleName}${if (t2Promo != t2) s" (↑${t2.simpleName})" else ""}"
      )
    }
    t1Promo
  }
}
