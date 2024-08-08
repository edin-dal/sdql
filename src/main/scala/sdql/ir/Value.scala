package sdql
package ir

case class DenseInt(i: Int, size: Int)
case class DateValue(v: Int)
case class RecordValue(values: Seq[(String, Value)])
case object ZeroValue

object Value {
  def toString(v: Value): String = v match {
    case b: Boolean => b.toString
    case DateValue(v) =>
      val s = v.toString
      s"${s.substring(0, 4)}-${s.substring(4, 6)}-${s.substring(6, 8)}"
    case s: String => "\"" + s + "\""
    case d: Double => d.toString
    case i: Int    => i.toString
    case m: Map[_, _] =>
      m.map(kv => s"${toString(kv._1)} -> ${toString(kv._2)}").mkString("{", ", ", "}")
    case RecordValue(vals) =>
      vals.map(fv => s"${fv._1} = ${fv._2}").mkString("<", ", ", ">")
    case ZeroValue =>
      ZeroValue.toString
    case _ => raise(s"Doesn't know how to convert `$v` to string")
  }
  def normalize(v: Value): Value =
    raise("normalize not supported yet")
}
