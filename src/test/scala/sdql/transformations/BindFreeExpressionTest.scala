package sdql.transformations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.backend.codegen.resultName
import sdql.frontend.*
import sdql.ir.Sym

class BindFreeExpressionTest extends AnyFlatSpec with Matchers {
  it should "bind free expression" in {
    val e       = sdql"let x = y in z"
    val rewrite = sdql"let x = y in let ${Sym(resultName)} = z in {}"
    BindFreeExpression(e) should be(rewrite)
  }
}
