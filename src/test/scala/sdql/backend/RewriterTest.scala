package sdql
package backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
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

// TODO
//class RemoveRecordGetTest extends AnyFlatSpec with Matchers {
//  it should "remove record get" in {
//    val e       = sdql"let record = <a = 0, b = 1> in record(2)"
//    val rewrite = sdql"let record = <a = 0, b = 1> in record.b"
//    RemoveRecordGet(e) should be(rewrite)
//  }
//
//  it should "not remove record get when field name is duplicate" in {
//    val e = sdql"let record = <_ = 0, _ = 1> in record(2)"
//    RemoveRecordGet(e) should be(e)
//  }
//
//  it should "remove record get load" in {
//    val e       = sdql"""
//                 let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
//                 lineitem(1)
//                 """
//    val rewrite = sdql"""
//                 let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
//                 lineitem.l_extendedprice
//                 """
//    RemoveRecordGet(e) should be(rewrite)
//  }
//}

class SkipUnusedColumnsTest extends AnyFlatSpec with Matchers {
  it should "skip unused columns" in {
    val e       = sdql"""
               let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
               sum(<i,_> <- range(lineitem.size))
                   1
               """
    val rewrite = sdql"""
               let lineitem =
                   load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl", {"l_extendedprice"})
               sum(<i,_> <- range(lineitem.size))
                   1
               """
    SkipUnusedColumns(e) should be(rewrite)
  }

  it should "skip unused columns existing" in {
    val e       = sdql"""
               let lineitem =
                   load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl", {"size"})
               1
               """
    val rewrite = sdql"""
               let lineitem =
                   load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl", {"size", "l_extendedprice"})
               1
               """
    SkipUnusedColumns(e) should be(rewrite)
  }
}

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
                   { mk_tuple.movie_id -> @vecdict { mk_tuple -> 1 } }
               """
    val rewrite = sdql"""
               let mk =
                   load[<id: @vec {int -> int}, movie_id: @vec {int -> int}, keyword_id: @vec {int -> int}>
                       ]("foo/bar.csv")
               sum(<i,_> <- range(mk.size))
                   { mk.movie_id(i) -> @vecdict {
                       < id=mk.id(i), movie_id=mk.movie_id(i), keyword_id=mk.keyword_id(i) > -> 1
                   } }
               """
    RemoveIntermediateTuples(e) should be(rewrite)
  }
}
