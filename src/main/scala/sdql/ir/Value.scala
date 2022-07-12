package sdql
package ir

case class DenseInt(i: Int, size: Int)
case class DateValue(v: Int)
case class RecordValue(values: Seq[(String, Any)])
case object ZeroValue