package sdql
package backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import sdql.frontend._

class RewriterTest extends AnyFlatSpec with Matchers {
  it should "remove intermediate tuple" in {
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

  it should "remove intermediate tuple TPCH" in {
    val e = sdql"""
               let lineitem = load[<l_extendedprice: @vector {int -> double}, size: int>]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   let li = <l_extendedprice=lineitem.l_extendedprice(i)>
                   li.l_extendedprice
               """
    val rewrite = sdql"""
               let lineitem = load[<l_extendedprice: @vector {int -> double}, size: int>]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   lineitem.l_extendedprice(i)
               """
    Rewriter(e) should be (rewrite)
  }

  it should "remove intermediate tuple JOB" in {
    val e = sdql"""
               let mk =
                   load[<id: @vector {int -> int}, movie_id: @vector {int -> int}, keyword_id: @vector {int -> int}>
                       ]("foo/bar.csv")
               sum(<i,_> <- range(mk.size))
                   let mk_tuple = < id=mk.id(i), movie_id=mk.movie_id(i), keyword_id=mk.keyword_id(i) >
                   { mk_tuple.movie_id -> @vecdict { mk_tuple -> 1 } }
               """
    val rewrite = sdql"""
               let mk =
                   load[<id: @vector {int -> int}, movie_id: @vector {int -> int}, keyword_id: @vector {int -> int}>
                       ]("foo/bar.csv")
               sum(<i,_> <- range(mk.size))
                   { mk.movie_id(i) -> @vecdict {
                       < id=mk.id(i), movie_id=mk.movie_id(i), keyword_id=mk.keyword_id(i) > -> 1
                   } }
               """
    Rewriter(e) should be (rewrite)
  }
}
