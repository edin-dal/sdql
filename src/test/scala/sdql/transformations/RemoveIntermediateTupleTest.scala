package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import sdql.frontend.*

class RemoveIntermediateTupleTest extends AnyFlatSpec with Matchers {
  it should "remove intermediate tuples" in {
    val e       = sdql"""
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
    RemoveIntermediateTuples(e) should be(rewrite)
  }

  it should "remove intermediate tuples TPCH" in {
    val e       = sdql"""
               let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   let li = <l_extendedprice=lineitem.l_extendedprice(i)>
                   li.l_extendedprice
               """
    val rewrite = sdql"""
               let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   lineitem.l_extendedprice(i)
               """
    RemoveIntermediateTuples(e) should be(rewrite)
  }

  it should "remove intermediate tuples GJ" in {
    val e       = sdql"""
               let mk =
                   load[<id: @vec {int -> int}, movie_id: @vec {int -> int}, keyword_id: @vec {int -> int}>
                       ]("foo/bar.csv")
               sum(<i,_> <- range(mk.size))
                   let mk_tuple = < id=mk.id(i), movie_id=mk.movie_id(i), keyword_id=mk.keyword_id(i) >
                   { mk_tuple.movie_id -> @smallvecdict(0) { mk_tuple -> 1 } }
               """
    val rewrite = sdql"""
               let mk =
                   load[<id: @vec {int -> int}, movie_id: @vec {int -> int}, keyword_id: @vec {int -> int}>
                       ]("foo/bar.csv")
               sum(<i,_> <- range(mk.size))
                   { mk.movie_id(i) -> @smallvecdict(0) {
                       < id=mk.id(i), movie_id=mk.movie_id(i), keyword_id=mk.keyword_id(i) > -> 1
                   } }
               """
    RemoveIntermediateTuples(e) should be(rewrite)
  }
}
