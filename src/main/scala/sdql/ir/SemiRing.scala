package sdql
package ir

import scala.annotation.nowarn

abstract class TropicalSemiRing[T](val kind: TropicalSemiRingType, val value: Option[T])
object TropicalSemiRing {
  def apply(kind: TropicalSemiRingType, value: Double): TropicalSemiRing[Double] = kind match {
    case MinSumSemiRingType  => MinSumSemiRing(Some(value))
    case MaxSumSemiRingType  => MaxSumSemiRing(Some(value))
    case MinProdSemiRingType => MinProdSemiRing(Some(value))
    case MaxProdSemiRingType => MaxProdSemiRing(Some(value))
    case t                   => raise(s"unexpected: ${t.prettyPrint}")
  }
  @nowarn
  def unapply(e: Any): Option[(TropicalSemiRingType, Option[Double])]            = e match {
    case t: TropicalSemiRing[Double] => Some((t.kind, t.value))
    case _                           => None
  }
  val MinSumSemiRingType: TropicalSemiRingType                                   = TropicalSemiRingType(isMax = false, isProd = false)
  val MaxSumSemiRingType: TropicalSemiRingType                                   = TropicalSemiRingType(isMax = true, isProd = false)
  val MinProdSemiRingType: TropicalSemiRingType                                  = TropicalSemiRingType(isMax = false, isProd = true)
  val MaxProdSemiRingType: TropicalSemiRingType                                  = TropicalSemiRingType(isMax = true, isProd = true)
}

case class MinSumSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MinSumSemiRingType, value)
case class MaxSumSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MaxSumSemiRingType, value)
case class MinProdSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MinProdSemiRingType, value)
case class MaxProdSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MaxProdSemiRingType, value)

// type isn't known at time of parsing, it can be populated later by type inference
case class TropicalSemiRingType(isMax: Boolean, isProd: Boolean, tp: Option[Type] = None)
    extends CustomSemiRingType(
      s"${if (isMax) "max" else "min"}_${if (isProd) "prod" else "sum"}",
      Seq(isMax, isProd, tp)
    )
object TropicalSemiRingType {
  def apply(name: String): TropicalSemiRingType                        = name match {
    case "min_sum" | "mnsm"  => TropicalSemiRingType(isMax = false, isProd = false)
    case "max_sum" | "mxsm"  => TropicalSemiRingType(isMax = true, isProd = false)
    case "min_prod" | "mnpr" => TropicalSemiRingType(isMax = false, isProd = true)
    case "max_prod" | "mxpr" => TropicalSemiRingType(isMax = true, isProd = true)
  }
  def apply(name: String, tp: Type): TropicalSemiRingType              = this.pack(this.apply(name), tp)
  def pack(tsrt: TropicalSemiRingType, tp: Type): TropicalSemiRingType = tsrt match {
    case TropicalSemiRingType(isMax, isProd, None)          => TropicalSemiRingType(isMax, isProd, Some(tp))
    case TropicalSemiRingType(_, _, Some(tpe)) if tpe != tp => raise(s"$tpe ≠ $tp")
    case _                                                  => tsrt
  }
  def unpack(tp: Type): Type                                           = tp match {
    case TropicalSemiRingType(_, _, Some(tp))    => tp
    case tsrt @ TropicalSemiRingType(_, _, None) => raise(s"${tsrt.simpleName} is missing type information")
    case _                                       => tp
  }
}

case class EnumSemiRing[T](kind: EnumSemiRingType, value: EnumSemiRingValue[T])
sealed trait EnumSemiRingValue[+T] {
  def nonEmpty: Boolean = this match {
    case SingletonEnumSemiRing(_) => true
    case _                        => false
  }
  def get: T            = this match {
    case SingletonEnumSemiRing(v) => v
    case _                        => raise("not implemented")
  }
}
case object TopEnumSemiRing extends EnumSemiRingValue[Nothing]
case object BottomEnumSemiRing                 extends EnumSemiRingValue[Nothing]
case class SingletonEnumSemiRing[+T](value: T) extends EnumSemiRingValue[T]

case class EnumSemiRingType(tp: Type) extends CustomSemiRingType("enum", Seq(tp))

case class NullableSemiRing[T](kind: NullableSemiRingType, value: Option[T])

case class NullableSemiRingType(tp: Type) extends CustomSemiRingType("nullable", Seq(tp))
