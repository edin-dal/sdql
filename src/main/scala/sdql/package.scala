package object sdql {
  import ir.*
  type Field = String
  type Stmt  = (Sym, Exp)
  val Block              = LetBindingNStriped
  def raise(msg: String) = throw new Exception(s"Error: $msg")
}
