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
  def unapply(e: Any): Option[(TropicalSemiRingType, Option[Double])] = e match {
    case t: TropicalSemiRing[Double] => Some((t.kind, t.value))
    case _                           => None
  }
  val MinSumSemiRingType: TropicalSemiRingType  = TropicalSemiRingType(isMax = false, isProd = false, RealType)
  val MaxSumSemiRingType: TropicalSemiRingType  = TropicalSemiRingType(isMax = true, isProd = false, RealType)
  val MinProdSemiRingType: TropicalSemiRingType = TropicalSemiRingType(isMax = false, isProd = true, RealType)
  val MaxProdSemiRingType: TropicalSemiRingType = TropicalSemiRingType(isMax = true, isProd = true, RealType)
}

case class MinSumSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MinSumSemiRingType, value)
case class MaxSumSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MaxSumSemiRingType, value)
case class MinProdSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MinProdSemiRingType, value)
case class MaxProdSemiRing(override val value: Option[Double])
    extends TropicalSemiRing[Double](TropicalSemiRing.MaxProdSemiRingType, value)

case class TropicalSemiRingType(isMax: Boolean, isProd: Boolean, tp: Type)
    extends CustomSemiRingType(s"${if (isMax) "max" else "min"}_${if (isProd) "prod" else "sum"}",
                               Seq(isMax, isProd, tp))
object TropicalSemiRingType {
  def apply(name: String): TropicalSemiRingType = name match {
    case "min_sum" | "mnsm"  => TropicalSemiRingType(isMax = false, isProd = false, RealType)
    case "max_sum" | "mxsm"  => TropicalSemiRingType(isMax = true, isProd = false, RealType)
    case "min_prod" | "mnpr" => TropicalSemiRingType(isMax = false, isProd = true, RealType)
    case "max_prod" | "mxpr" => TropicalSemiRingType(isMax = true, isProd = true, RealType)
  }
}

case class EnumSemiRing[T](kind: EnumSemiRingType, value: EnumSemiRingValue[T])
sealed trait EnumSemiRingValue[+T] {
  def nonEmpty: Boolean = this match {
    case SingletonEnumSemiRing(_) => true
    case _                        => false
  }
  def get: T = this match {
    case SingletonEnumSemiRing(v) => v
    case _                        => ???
  }
}
case object TopEnumSemiRing                    extends EnumSemiRingValue[Nothing]
case object BottomEnumSemiRing                 extends EnumSemiRingValue[Nothing]
case class SingletonEnumSemiRing[+T](value: T) extends EnumSemiRingValue[T]

case class EnumSemiRingType(tp: Type) extends CustomSemiRingType("enum", Seq(tp))

case class NullableSemiRing[T](kind: NullableSemiRingType, value: Option[T])

case class NullableSemiRingType(tp: Type) extends CustomSemiRingType("nullable", Seq(tp))
