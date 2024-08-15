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
  val START_ID             = 1
  private var lastId       = START_ID.toLong

  /** Get a fresh symbol, i.e. xi, where i is the smallest number not used. */
  def fresh: Sym =
    fresh(DEFAULT_NAME)
  def fresh(name: String): Sym = {
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
case class DictNode(map: Seq[(Exp, Exp)], hint: DictHint = NoHint) extends Exp {
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
case object NoHint                       extends DictHint
case class SmallVecDict(size: Int)       extends DictHint
case object VecDict                      extends DictHint
case object VecDicts                     extends DictHint
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

/**
 * This object models the multiplication of a sequence of expressions. It can
 * either apply the multiplication operation to the sequence, or extract the
 * sequence of expressions (unapply) from a multiplication expression.
 */
object MultN {
  def apply(seq: Seq[Exp]): Exp = seq match {
    case Seq(e)            => e
    case Seq(e1, e2)       => Mult(e1, e2)
    case _ if seq.size > 2 => Mult(seq.head, MultN(seq.tail))
    case _                 => raise(s"MultN with size ${seq.size}")
  }
  def unapply(exp: Exp): Option[Seq[Exp]] = exp match {
    case Mult(e1, e2) =>
      (MultN.unapply(e1), MultN.unapply(e2)) match {
        case (Some(s1), Some(s2)) => Some(s1 ++ s2)
        case (None, Some(s2))     => Some(e1 +: s2)
        case (Some(s1), None)     => Some(s1 :+ e2)
        case _                    => Some(Seq(e1, e2))
      }
    case _ => None
  }
}

object MultNStriped {
  def unapply(exp: Exp): Option[Seq[Exp]] = exp match {
    case MultN(seq) => Some(seq)
    case _          => Some(Seq(exp))
  }
}

object SetNode {
  def apply(es: Seq[Exp]): DictNode               = DictNode(es.map(x => x -> Const(1)))
  def fromSkipColsSet(set: Set[String]): DictNode = SetNode(set.map(Const.apply).toSeq)
}

object PairNode {
  import PairType.*
  def apply(_1: Exp, _2: Exp): Exp = RecNode(Seq((FST -> _1), (SND -> _2)))
  def unapply(e: Exp): Option[(Exp, Exp)] = e match {
    case RecNode(Seq(a1, a2)) if a1._1 == FST && a2._1 == SND => Some(a1._2 -> a2._2)
    case _                                                    => None
  }
}

object Fst {
  def apply(e: Exp): Exp = FieldNode(e, PairType.FST)
  def unapply(e: Exp): Option[Exp] = e match {
    case FieldNode(e1, PairType.FST) => Some(e1)
    case _                           => None
  }
}

object Snd {
  def apply(e: Exp): Exp = FieldNode(e, PairType.SND)
  def unapply(e: Exp): Option[Exp] = e match {
    case FieldNode(e1, PairType.SND) => Some(e1)
    case _                           => None
  }
}

object ProjectionNode {
  import PairType.*
  def unapply(e: Exp): Option[(Exp, Int)] = e match {
    case FieldNode(e1, FST) => Some(e1 -> 1)
    case FieldNode(e1, SND) => Some(e1 -> 2)
    case _                  => None
  }
}

/**
 * This object models the addition of a sequence of expressions. It can either
 * apply the addition operation to the sequence, or extract the sequence of
 * expressions (unapply) from an addition expression.
 */
object AddN {
  def apply(seq: Seq[Exp]): Exp = seq match {
    case Seq(e)            => e
    case Seq(e1, e2)       => Add(e1, e2)
    case _ if seq.size > 2 => Add(seq.head, AddN(seq.tail))
    case _                 => ???
  }
  def unapply(exp: Exp): Option[Seq[Exp]] = exp match {
    case Add(e1, e2) =>
      (AddN.unapply(e1), AddN.unapply(e2)) match {
        case (Some(s1), Some(s2)) => Some(s1 ++ s2)
        case (None, Some(s2))     => Some(e1 +: s2)
        case (Some(s1), None)     => Some(s1 :+ e2)
        case _                    => Some(Seq(e1, e2))
      }
    case _ => None
  }
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
  def unapply(exp: Exp): Res =
    rec(exp, None)
  //   unapplyOpt(exp)

  @tailrec def rec(exp: Exp, res: Res): Res = exp match {
    case LetBinding(x, e1, e2) =>
      rec(e2, res match {
        case Some((seq, _)) => Some((seq :+ (x -> e1)) -> e2)
        case None           => Some(Seq(x -> e1) -> e2)
      })
    case _ => res
  }

  // def unapplyOpt(exp: Exp): Res = {
  //   val seq = scala.collection.mutable.ListBuffer[(Sym, Exp)]()
  //   var current = exp
  //   var stop = false
  //   while(!stop) {
  //     current match {
  //       case LetBinding(x, e1, e2) =>
  //         seq += x -> e1
  //         current = e2
  //       case _ =>
  //         stop = true
  //     }
  //   }
  //   if(seq.isEmpty)
  //     None
  //   else
  //     Some(seq.toSeq -> current)
  // }
}

object LetBindingNStriped {
  def unapply(t: Exp): Option[(Seq[(Sym, Exp)], Exp)] = t match {
    case LetBindingN(xs, body) => Some(xs    -> body)
    case _                     => Some(Seq() -> t)
  }
}

// object Contains {
//   def apply(dict: Exp, k: Exp, vtp: Type): Exp = Cmp(Get(dict, k), Zero(vtp), "==")
//   def unapply(e: Exp): Option[(Exp, Exp, Type)] = e match {
//     case Cmp(Get(dict, k), Zero(vtp), "==") => Some((dict, k, vtp))
//     case _ => None
//   }
// }

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
