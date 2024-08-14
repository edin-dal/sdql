package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.frontend.*

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
