package sdql
package backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import sdql.frontend._

class RewriterTest extends AnyFlatSpec with Matchers {
  it should "remove intermediate tuple - simple case" in {
    val e = sdql"""
               let i = 0
               let x = <inner = <_ = 0>>
               let y = <inner = x.inner(i)>
               y.inner
               """
    val rewrite = sdql"""
               let i = 0
               let x = <inner = <_ = 0>>
               x.inner(i)
               """
    Rewriter(e) should be (rewrite)
  }

  it should "remove intermediate tuple - complex case" in {
    val e = sdql"""
               let lineitem = load_cstore[{<l_extendedprice: double> -> int}]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   let li = <l_extendedprice=lineitem.l_extendedprice(i)>
                   li.l_extendedprice
               """
    val rewrite = sdql"""
               let lineitem = load_cstore[{<l_extendedprice: double> -> int}]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   lineitem.l_extendedprice(i)
               """
    Rewriter(e) should be (rewrite)
  }
}
