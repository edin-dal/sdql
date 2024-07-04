package sdql
package ir

sealed trait Type {
  def =~=(o: Type): Boolean = equals(o)
  def isScalar: Boolean = ScalarType.isScalar(this)
  def simpleName: String = {
    val name = this.getClass.getSimpleName
    if (name.endsWith("$")) name.dropRight(1) else name
  }
}
case object BottomType extends Type
case class StringType(maxLen: Option[Int] = None) extends Type
case object RealType extends Type
// Used as the underlying type of a RangeNode, and keys for DictNodes that will generate arrays
case class DenseIntType(n: Int) extends Type
case object BoolType extends Type
case object IntType extends Type
case object CharType extends Type
case object DateType extends Type
case class DictType(key: Type, value: Type, hint: DictCodegenHint = DictNoHint()) extends Type
case class RecordType(attrs: Seq[Attribute]) extends Type {
  override def equals(o: Any): Boolean = o match {
    case RecordType(attrs2) if attrs.size == attrs2.size =>
      // attrs.zip(attrs2).forall(x => x._1.name == x._2.name)
      attrs.zip(attrs2).forall(x => x._1.name == x._2.name && x._1.tpe == x._2.tpe)
    case _ => false
  }
  override def =~=(o: Type): Boolean = o match {
    case RecordType(attrs2) if attrs.size == attrs2.size =>
      attrs.zip(attrs2).forall(x => x._1.name == x._2.name && x._1.tpe =~= x._2.tpe)
    case _ => false
  }
  override def hashCode(): Int =
    attrs.map(_.name).hashCode()
  def indexOf(name: Field): Option[Int] = {
    val names = attrs.map(_.name)
    assert(names.diff(names.distinct).isEmpty)
    names.zipWithIndex.find(_._1 == name).map(_._2)
  }
}

sealed trait DictCodegenHint;
case class DictNoHint() extends DictCodegenHint
case class DictVectorHint() extends DictCodegenHint

object TupleType {
  def index(idx: Int): String = s"__$idx"
  // FIXME
  def isIndex(s: String): Boolean = s.startsWith("__")
  def apply(tps: Seq[Type]): RecordType = RecordType(tps.zipWithIndex.map(ti => Attribute(index(ti._2), ti._1)))
  def unapply(tp: Type): Option[Seq[Type]] = tp match {
    case RecordType(attrs) if attrs.forall(a => isIndex(a.name)) =>
      Some(attrs.map(_.tpe))
    case _ => None
  }
}

// corresponds to std::pair of C++
object PairType {
  val FST = "_1"
  val SND = "_2"
  def apply(_1: Type, _2: Type): RecordType = RecordType(Seq(Attribute(FST, _1), Attribute(SND, _2)))
  def unapply(tp: Type): Option[(Type, Type)] = tp match {
    case RecordType(Seq(a1, a2)) if a1.name == FST && a2.name == SND => Some(a1.tpe -> a2.tpe)
    case _ => None
  }
}

object VarCharType {
  def apply(maxLen: Int): Type = StringType(Some(maxLen))
}
object DecimalType {
  def apply(i: Int): Type = RealType
}

object ScalarType {
  def unapply(tp: Type): Option[Type] = if(isScalar(tp)) Some(tp) else None
  def isScalar(tp: Type) = tp match {
    case _: DenseIntType | RealType | IntType | _: StringType | DateType | CharType | BoolType => true
    case _ => false
  }
}

case class Attribute(name: Field, tpe: Type)

abstract class CustomSemiRingType(val name: String, val params: Seq[Any]) extends Type
