package object sdql {
  type Field = String

  def raise(msg: String): Nothing = throw new Exception(s"Error: $msg")
}
