package sdql
package ir

import munit.Assertions.munitPrint

import scala.annotation.tailrec

/**
 * This trait models expressions used in the SDQL language, without also
 * computing them. It can be used to generate an abstract syntax tree of a
 * given program.
 */
sealed trait Exp {
  def prettyPrint: String = munitPrint(this)

  def simpleName: String = {
    val name = this.getClass.getSimpleName
    if (name.endsWith("$")) name.dropRight(1) else name
  }
}

/**
 * This class models a symbol, e.g. "x1" or "x2", and it also includes options
 * to get a fresh symbol, or to reset the used symbol counter (start over from
 * x1).
 */
case class Sym(name: String) extends Exp
object Sym {
  private val DEFAULT_NAME = "x"
  private val START_ID     = 1
  private var lastId       = START_ID.toLong

  /** Get a fresh symbol, i.e. xi, where i is the smallest number not used. */
  def fresh: Sym = fresh(DEFAULT_NAME)
  private def fresh(name: String): Sym = {
    val cur = freshId
    Sym(s"$name$cur")
  }
  private def freshId: Long = {
    val cur = lastId
    lastId += 1
    cur
  }

  /** Reset the symbol index, such that the next fresh symbol will be x1. */
  def reset(): Unit =
    lastId = START_ID.toLong
}

/**
 * A constant
 * @param v integer, double, string, boolean
 */
case class Const(v: Any) extends Exp

/**
 * A record (tuple), with field labels
 * @param values a sequence of expression values with a field label
 */
case class RecNode(values: Seq[(Field, Exp)]) extends Exp {
  def apply(name: Field): Option[Exp] = values.find(_._1 == name).map(_._2)

  def concat(other: RecNode): RecNode = other match {
    case RecNode(otherValues) =>
      val (thisMap, otherMap) = values.toMap -> otherValues.toMap
      val common              = values.filter(x1 => otherMap.contains(x1._1)).map(x1 => (x1._1, x1._2, otherMap(x1._1)))
      if (common.isEmpty)
        RecNode(values ++ otherValues)
      else if (common.forall(x => x._2 == x._3))
        RecNode(values ++ otherValues.filter(x2 => !thisMap.contains(x2._1)))
      else
        raise(s"`concat(${this.prettyPrint}, ${other.prettyPrint})` with different values for the same field name")
  }
}

/**
 * A dictionary that maps expressions to other expressions
 * @param map a dictionary from expression to other expressions
 */
case class DictNode(map: Seq[(Exp, Exp)], hint: DictHint = PHmap()) extends Exp {
  @tailrec
  final def getInnerDict: DictNode = this match {
    case DictNode(Seq((_, dict: DictNode)), _) => dict.getInnerDict
    case DictNode(map, _) if map.length != 1   => raise(s"unsupported: $this")
    case _                                     => this
  }
  def isSetNode: Boolean = this == SetNode(this.map.map(_._1))
  def toSkipColsSet: Set[String] =
    this.map.map(_._1).map(k => (k: @unchecked) match { case Const(s: String) => s }).toSet
}
sealed trait DictHint
case class PHmap(e: Option[Exp] = None)  extends DictHint
case class SmallVecDict(size: Int)       extends DictHint
case class SmallVecDicts(size: Int)      extends DictHint
case class Vec(size: Option[Int] = None) extends DictHint

/**
 * Integer numbers between 0 and n
 * @param e an expression evaluating to n
 */
case class RangeNode(e: Exp) extends Exp

/**
 * Addition of two expressions
 * @param e1 exp1
 * @param e2 exp2
 */
case class Add(e1: Exp, e2: Exp) extends Exp

/**
 * Multiplication of two expressions
 * @param e1 exp1
 * @param e2 exp2
 */
case class Mult(e1: Exp, e2: Exp) extends Exp

/**
 * Negative of an expression
 * @param e exp1
 */
case class Neg(e: Exp) extends Exp

/**
 * Comparison of two expressions
 * @param e1 exp1
 * @param e2 exp2
 * @param cmp Comparison operator
 */
case class Cmp(e1: Exp, e2: Exp, cmp: String) extends Exp

/**
 * Conditional statement
 * @param cond condition
 * @param thenp expression if true
 * @param elsep expression if false
 */
case class IfThenElse(cond: Exp, thenp: Exp, elsep: Exp) extends Exp
case class FieldNode(e: Exp, f: Field)                   extends Exp
case class Sum(key: Sym, value: Sym, e1: Exp, body: Exp) extends Exp
case class Get(e1: Exp, e2: Exp)                         extends Exp
case class Concat(e1: Exp, e2: Exp)                      extends Exp
case class LetBinding(x: Sym, e1: Exp, e2: Exp) extends Exp {
  override def hashCode(): Int = this match {
    case LetBindingN(xs, res) =>
      xs.map(xe => xe._1.hashCode() + xe._2.hashCode()).sum + res.hashCode()
    case _ => super.hashCode()
  }
}
case class Load(path: String, tp: Type, skipCols: DictNode = SetNode(Nil)) extends Exp

case class Promote(tp: Type, e: Exp)              extends Exp
case class External(name: String, args: Seq[Exp]) extends Exp
case class Unique(e: Exp)                         extends Exp

object SetNode {
  def apply(es: Seq[Exp]): DictNode               = DictNode(es.map(x => x -> Const(1)))
  def fromSkipColsSet(set: Set[String]): DictNode = SetNode(set.map(Const.apply).toSeq)
}

/**
 * This object models a sequence of let bindings, where the bindings can either
 * be applied in an expression body, or unapplied and returned from a given
 * expression.
 */
object LetBindingN {
  def apply(bindings: Seq[(Sym, Exp)], body: Exp): Exp =
    bindings.foldRight(body)((cur, acc) => LetBinding(cur._1, cur._2, acc))
  private type Res = Option[(Seq[(Sym, Exp)], Exp)]
  def unapply(exp: Exp): Res = rec(exp, None)

  @tailrec def rec(exp: Exp, res: Res): Res = exp match {
    case LetBinding(x, e1, e2) =>
      rec(e2, res match {
        case Some((seq, _)) => Some((seq :+ (x -> e1)) -> e2)
        case None           => Some(Seq(x -> e1) -> e2)
      })
    case _ => res
  }
}

object And { def apply(a: Exp, b: Exp): Exp = IfThenElse(a, b, Const(false)) }

object Or { def apply(a: Exp, b: Exp): Exp = IfThenElse(a, Const(true), b) }

object Not {
  def apply(a: Exp): Exp = IfThenElse(a, Const(false), Const(true))
  def unapply(e: Exp): Option[Exp] = e match {
    case IfThenElse(a, Const(false), Const(true)) => Some(a)
    case _                                        => None
  }
}

object SingleDict {
  def apply(k: Exp, v: Exp): Exp = DictNode(Seq((k, v)))
  def unapply(exp: Exp): Option[(Exp, Exp)] = exp match {
    case DictNode(Seq((k, v)), _) => Some((k, v))
    case _                        => None
  }
}

sealed trait LLQL
case class Initialise(tpe: Type, agg: Aggregation, e: Sum) extends Exp with LLQL // TODO infer aggregation from type
case class Update(e: Exp, agg: Aggregation, dest: Sym)     extends Exp with LLQL
case class Modify(e: Exp, dest: Sym)                       extends Exp with LLQL
