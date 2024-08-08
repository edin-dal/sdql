package sdql
package ir

abstract class ExternalFactory(symbol: String) {
  val SYMBOL: String = symbol
  def apply(es: Exp*): Exp = External(symbol, es.toSeq)
  def unapplySeq(e: Exp): Option[Seq[Exp]] = e match {
    case External(sym, seq) if sym == symbol =>
      Some(seq)
    case _ =>
      None
  }
}
object ExternalFunctions {
  object TopN extends ExternalFactory("TopN")
  object ConstantString extends ExternalFactory("ConstantString")
  object StrContains extends ExternalFactory("StrContains")
  object StrStartsWith extends ExternalFactory("StrStartsWith")
  object StrEndsWith extends ExternalFactory("StrEndsWith")
  object StrContainsN extends ExternalFactory("StrContainsN")
  object SubString extends ExternalFactory("SubString")
  object StrIndexOf extends ExternalFactory("StrIndexOf")
  object FirstIndex extends ExternalFactory("FirstIndex")
  object LastIndex extends ExternalFactory("LastIndex")
  object Year extends ExternalFactory("Year")
  object CStore extends ExternalFactory("CStore")
  object ParseDate extends ExternalFactory("ParseDate")
  object Inv extends ExternalFactory("inv")
  object Log extends ExternalFactory("log")
  object MaxValue extends ExternalFactory("MaxValue")
  object Size extends ExternalFactory("Size")
  object Limit extends ExternalFactory("Limit")
}
