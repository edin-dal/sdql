package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.frontend.*
import sdql.ir.*

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

  it should "remove aliases handling local scopes" in {
    val e       = sdql"""
               let x = 0
               let y = 1
               if (true) then
                   let alias = x
                   alias + y
               else
                   let alias = y
                   x + alias
               """
    val rewrite = sdql"""
               let x = 0
               let y = 1
               if (true) then
                   x + y
               else
                   x + y
               """
    RemoveAliases(e) should be(rewrite)
  }
}

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

  it should "skip only unused columns when alias and record get" in {
    val e       = sdql"""
               let lineitem = load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl")
               let alias = lineitem
               sum(<i,_> <- range(alias(2)))
                   1
               """
    val rewrite = sdql"""
               let lineitem =
                   load[<l_extendedprice: @vec {int -> double}, size: int>]("foo/bar.tbl", {"l_extendedprice"})
               sum(<i,_> <- range(lineitem.size))
                   1
               """
    (RemoveAliases ++ RemoveRecordGet ++ SkipUnusedColumns)(e) should be(rewrite)
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

class BindFreeExpressionTest extends AnyFlatSpec with Matchers {
  it should "bind free expression" in {
    val e       = sdql"let x = y in z"
    val rewrite = sdql"let x = y in let ${Sym(resultName)} = z in {}"
    BindFreeExpression(e) should be(rewrite)
  }
}

class LowerToLLQLTest extends AnyFlatSpec with Matchers {
  it should "lower sum" in {
    //                            i
    //         sum(<i, _> <- {2}) i
    // let x = sum(<i, _> <- {2}) i in x
    val body = Sym("i")
    val sum  = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), body)
    val e    = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) i in x")

    val bodyRewrite = Update(body, SumAgg, Sym("x"))
    val sumRewrite  = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), bodyRewrite)
    val rewrite     = LetBinding(Sym("x"), Initialise(IntType, sumRewrite), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }

  it should "lower nested sum" in {
    //                                               i * j
    //                            sum(<j, _> <- {3}) i * j
    //         sum(<i, _> <- {2}) sum(<j, _> <- {3}) i * j in x
    // let x = sum(<i, _> <- {2}) sum(<j, _> <- {3}) i * j in x
    val body  = Mult(Sym("i"), Sym("j"))
    val inner = Sum(Sym("j"), Sym("_"), SetNode(Seq(Const(3))), body)
    val sum   = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), inner)
    val e     = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) sum(<j, _> <- {3}) i * j in x")

    val bodyRewrite  = Update(body, SumAgg, Sym("x"))
    val innerRewrite = Sum(Sym("j"), Sym("_"), SetNode(Seq(Const(3))), bodyRewrite)
    val sumRewrite   = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), innerRewrite)
    val rewrite      = LetBinding(Sym("x"), Initialise(IntType, sumRewrite), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }

  it should "lower nested let sum" in {
    //                                                       j
    //                                    sum(<j, _> <- {3}) j
    //                            let k = sum(<j, _> <- {3}) j in k
    //         sum(<i, _> <- {2}) let k = sum(<j, _> <- {3}) j in k
    // let x = sum(<i, _> <- {2}) let k = sum(<j, _> <- {3}) j in k in x
    val body     = Sym("j")
    val innerSum = Sum(Sym("j"), Sym("_"), SetNode(Seq(Const(3))), body)
    val inner    = LetBinding(Sym("k"), innerSum, Sym("k"))
    val sum      = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), inner)
    val e        = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) let k = sum(<j, _> <- {3}) j in k in x")

    val bodyRewrite     = Update(body, SumAgg, Sym("k"))
    val innerSumRewrite = Sum(Sym("j"), Sym("_"), SetNode(Seq(Const(3))), bodyRewrite)
    val innerRewrite =
      LetBinding(Sym("k"), Initialise(IntType, innerSumRewrite), Update(Sym("k"), SumAgg, Sym("x")))
    val sumRewrite = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), innerRewrite)
    val rewrite    = LetBinding(Sym("x"), Initialise(IntType, sumRewrite), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }

  it should "lower semi-ring sum" in {
    //                            promote[min_sum](i)
    //         sum(<i, _> <- {2}) promote[min_sum](i)
    // let x = sum(<i, _> <- {2}) promote[min_sum](i) in x
    val body = Promote(TropicalSemiRingType("min_sum"), Sym("i"))
    val sum  = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), body)
    val e    = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) promote[min_sum](i) in x")

    val bodyRewrite = Update(body, MinAgg, Sym("x"))
    val sumRewrite  = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), bodyRewrite)
    val rewrite = LetBinding(
      Sym("x"),
      Initialise(TropicalSemiRingType("min_sum", IntType), sumRewrite),
      Sym("x"),
    )
    LowerToLLQL(e) should be(rewrite)
  }
}
