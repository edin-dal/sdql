package sdql
package ir

case class DenseInt(i: Int, size: Int)
// case class DateValue(v: Int)
object DateValue {
  def apply(v: Int): Int = v
  def unapply(v: Int): Option[Int] = Some(v)
}
case class RecordValue(values: Seq[(String, Any)])
case object ZeroValue