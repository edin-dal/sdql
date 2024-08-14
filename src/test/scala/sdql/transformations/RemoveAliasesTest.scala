package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.frontend.*

class RemoveAliasesTest extends AnyFlatSpec with Matchers {
  it should "remove aliases" in {
    val e       = sdql"""
               let original = 1
               let alias = original
               let aka = alias
               aka
               """
    val rewrite = sdql"""
               let original = 1
               original
               """
    RemoveAliases(e) should be(rewrite)
  }
}
