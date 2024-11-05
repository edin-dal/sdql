package sdql
package ir

sealed trait Type {
  def =~=(o: Type): Boolean = equals(o)
  def isScalar: Boolean     = ScalarType.isScalar(this)
  def prettyPrint: String   = this match {
    case DictType(kt, vt, _) =>
      s"{${kt.prettyPrint} -> ${vt.prettyPrint}}"
    case RecordType(attrs)   =>
      attrs.map(_.tpe.prettyPrint).mkString("<", ", ", ">")
    case _                   =>
      this.simpleName
  }
  def simpleName: String    = {
    val name = this.getClass.getSimpleName
    if (name.endsWith("$")) name.dropRight(1) else name
  }
}
case class StringType(maxLen: Option[Int] = None) extends Type
case object RealType                                                  extends Type
case object BoolType                                                  extends Type
case object IntType                                                   extends Type
case object LongType                                                  extends Type
case object DateType                                                  extends Type
case class DictType(key: Type, value: Type, hint: DictHint = PHmap()) extends Type
object SetType { def apply(key: Type): DictType = DictType(key, IntType) }
case class RecordType(attrs: Seq[Attribute]) extends Type {
  override def equals(o: Any): Boolean      = o match {
    case RecordType(attrs2) if attrs.size == attrs2.size =>
      attrs.zip(attrs2).forall(x => x._1.name == x._2.name && x._1.tpe == x._2.tpe)
    case _                                               => false
  }
  override def =~=(o: Type): Boolean        = o match {
    case RecordType(attrs2) if attrs.size == attrs2.size =>
      attrs.zip(attrs2).forall(x => x._1.name == x._2.name && x._1.tpe =~= x._2.tpe)
    case _                                               => false
  }
  override def hashCode(): Int              =
    attrs.map(_.name).hashCode()
  def indexOf(name: Field): Option[Int]     = {
    val names = attrs.map(_.name)
    assert(names.diff(names.distinct).isEmpty)
    names.zipWithIndex.find(_._1 == name).map(_._2)
  }
  def apply(name: Field): Option[Type]      = attrs.find(_.name == name).map(_.tpe)
  def concat(other: RecordType): RecordType = other match {
    case RecordType(attrs2) =>
      val (fs1m, fs2m) = attrs.map(x1 => (x1.name, x1.tpe)).toMap -> attrs2.map(x2 => (x2.name, x2.tpe)).toMap
      val common       =
        attrs.filter(x1 => fs2m.contains(x1.name)).map(x1 => (x1.name, x1.tpe, fs2m(x1.name)))
      if (common.isEmpty)
        RecordType(attrs ++ attrs2)
      else if (common.forall(x => x._2 == x._3))
        RecordType(attrs ++ attrs2.filter(x2 => !fs1m.contains(x2.name)))
      else
        raise(s"`concat(${this.prettyPrint}, ${other.prettyPrint})` with different values for the same field name")
  }
}

object VarCharType { def apply(maxLen: Int): Type = StringType(Some(maxLen)) }

object ScalarType {
  def unapply(tp: Type): Option[Type] = Some(tp).filter(isScalar)
  def isScalar(tp: Type): Boolean     = tp match {
    case RealType | IntType | _: StringType | DateType | BoolType => true
    case _                                                        => false
  }
}

case class Attribute(name: Field, tpe: Type)

abstract class CustomSemiRingType(val name: String, val params: Seq[Any]) extends Type

sealed trait Aggregation
case object SumAgg  extends Aggregation
case object ProdAgg extends Aggregation
case object MinAgg  extends Aggregation
case object MaxAgg  extends Aggregation

object Aggregation {
  def fromType(tp: Type): Aggregation = tp match {
    case TropicalSemiRingType(false, false, _) => MinAgg
    case TropicalSemiRingType(true, false, _)  => MaxAgg
    case TropicalSemiRingType(_, true, _)      => ProdAgg
    case _                                     => SumAgg
  }

  def fromExpression(e: Exp): Aggregation = e match {
    case Promote(tp, _) => Aggregation.fromType(tp)
    case _              => SumAgg
  }
}
