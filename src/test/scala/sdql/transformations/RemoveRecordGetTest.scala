package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.frontend.*

class RemoveRecordGetTest extends AnyFlatSpec with Matchers {
  it should "remove record get" in {
    val e       = sdql"let record = <a = 0, b = 1> in record(2)"
    val rewrite = sdql"let record = <a = 0, b = 1> in record.b"
    RemoveRecordGet(e) should be(rewrite)
  }

  it should "not remove record get when field name is duplicate" in {
    val e = sdql"let record = <_ = 0, _ = 1> in record(2)"
    RemoveRecordGet(e) should be(e)
  }

  it should "remove record get load" in {
    val e       = sdql"""
                 let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
                 lineitem(1)
                 """
    val rewrite = sdql"""
                 let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
                 lineitem.l_extendedprice
                 """
    RemoveRecordGet(e) should be(rewrite)
  }

  it should "remove record get mixed" in {
    val e       = sdql"""
                 let record = <a = 0, b = 1>
                 let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
                 record(2) * lineitem(1)
                 """
    val rewrite = sdql"""
                 let record = <a = 0, b = 1>
                 let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
                 record.b * lineitem.l_extendedprice
                 """
    RemoveRecordGet(e) should be(rewrite)
  }
}
