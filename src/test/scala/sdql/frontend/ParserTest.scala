package sdql
package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import sdql.ir.*

class ParserTest extends AnyFlatSpec with Matchers {

  "Parser" should "work for constant terms" in {
    sdql"true" should be(Const(true))
    sdql"false" should be(Const(false))
    sdql"52" should be(Const(52))
    sdql"-52" should be(Const(-52))
    sdql"52.1" should be(Const(52.1))
    sdql""" "foo" """ should be(Const("foo"))
    sdql"date(19890713)" should be(Const(DateValue(19890713)))
    assertThrows[Exception] {
      sdql"`foo`"
    }
  }

  it should "work for if then else" in {
    sdql"if true then 0 else 1" should be(IfThenElse(Const(true), Const(0), Const(1)))
    sdql"if (true) then (0) else (1)" should be(IfThenElse(Const(true), Const(0), Const(1)))
    sdql"if (!true) then (0) else (1)" should be(IfThenElse(Not(Const(true)), Const(0), Const(1)))
  }

  it should "work for letbinding" in {
    sdql"let x = 1 in 2" should be(LetBinding(Sym("x"), Const(1), Const(2)))
    sdql"let    x  =    (1) in    2" should be(LetBinding(Sym("x"), Const(1), Const(2)))
    sdql"let x_1 = 1 in 2" should be(LetBinding(Sym("x_1"), Const(1), Const(2)))
    sdql"""let X = 1 in 
           2""" should be(LetBinding(Sym("X"), Const(1), Const(2)))
    sdql"x__2" should be(Sym("x__2"))
    sdql"X__2" should be(Sym("X__2"))
    assertThrows[Exception] {
      sdql"let x = 1 (2)"
    }
  }

  it should "work for sum" in {
    sdql"sum(<k,v> <- X) v" should be(Sum(Sym("k"), Sym("v"), Sym("X"), Sym("v")))
    sdql"sum(<k,v> <- X) {k -> v}" should be(Sum(Sym("k"), Sym("v"), Sym("X"), SingleDict(Sym("k"), Sym("v"))))
  }

  it should "handle comments" in {
    val term = LetBinding(Sym("x"), Sym("y"), Sym("z"))
    sdql"/* comment for let */ let x = y in z" should be(term)
    sdql"let x = y in /* comment for let */ z" should be(term)
    sdql"let x = y in z /* cc */" should be(term)
    sdql"""// comment before
  let x = y in // comment middle 1
  // comment middle 2
  z
  """ should be(term)
    sdql"""/* comment before */
  let x /* comment inside */ = /* comment inside */ y in 
  /* comment middle */
  z
  """ should be(term)
  }

  it should "work for arith" in {
    sdql"2 * 3" should be(Mult(Const(2.0), Const(3.0)))
    sdql"2 + 3" should be(Add(Const(2.0), Const(3.0)))
    sdql"2 / 3" should be(Mult(Const(2.0), ExternalFunctions.Inv(Const(3.0))))
    sdql"2 - 3" should be(Add(Const(2.0), Neg(Const(3.0))))
    sdql"2 + 1 * 3" should be(Add(Const(2.0), Mult(Const(1.0), Const(3.0))))
    sdql"2 * 1 + 3" should be(Add(Mult(Const(2.0), Const(1.0)), Const(3.0)))
    sdql"(2 * 1) + 3" should be(Add(Mult(Const(2.0), Const(1.0)), Const(3.0)))
    sdql"-x + y" should be(Add(Neg(Sym("x")), Sym("y")))
    sdql"-x * y" should be(Mult(Neg(Sym("x")), Sym("y")))
    sdql"-(x + y)" should be(Neg(Add(Sym("x"), Sym("y"))))
    sdql"2 < 3" should be(Cmp(Const(2.0), Const(3.0), "<"))
    sdql"2 < 3 * 1" should be(Cmp(Const(2.0), Mult(Const(3.0), Const(1.0)), "<"))
    sdql"2 < (3 * 1)" should be(Cmp(Const(2.0), Mult(Const(3.0), Const(1.0)), "<"))
    sdql"2 * 3" should be(Mult(Const(2.0), Const(3.0)))
    sdql"2 ^ 0" should be(Const(1))
    sdql"2 ^ 1" should be(Const(2))
    sdql"2 ^ 2" should be(Mult(Const(2), Const(2)))
    sdql"2 ^ 3" should be(Mult(Mult(Const(2), Const(2)), Const(2)))
    sdql"2 && 3" should be(And(Const(2.0), Const(3.0)))
    sdql"2 || 3" should be(Or(Const(2.0), Const(3.0)))
    sdql"!2" should be(Not(Const(2.0)))
  }

  it should "work for set & dict" in {
    sdql"{}" should be(SetNode(Seq()))
    sdql"{ 1 }" should be(SetNode(Seq(Const(1))))
    sdql"{ x }" should be(SetNode(Seq(Sym("x"))))
    sdql"{  0   , 1   }" should be(SetNode(Seq(Const(0), Const(1))))
    sdql"{  x   , y   }" should be(SetNode(Seq(Sym("x"), Sym("y"))))
    sdql"{0  ->  1}" should be(DictNode(Seq(Const(0) -> Const(1))))
    sdql"{x  ->  y}" should be(DictNode(Seq(Sym("x") -> Sym("y"))))
    sdql"{x.z  ->  y}" should be(DictNode(Seq(FieldNode(Sym("x"), "z") -> Sym("y"))))
    sdql"{x  ->  y, z -> 1 }" should be(DictNode(Seq(Sym("x") -> Sym("y"), Sym("z") -> Const(1.0))))
    sdql"x(y)" should be(Get(Sym("x"), Sym("y")))
    sdql"x(y)(z)" should be(Get(Get(Sym("x"), Sym("y")), Sym("z")))
    sdql"x(   y)" should be(Get(Sym("x"), Sym("y")))
    sdql"x(y)  " should be(Get(Sym("x"), Sym("y")))
    sdql"range(3)" should be(RangeNode(Const(3)))
    sdql"range(x)" should be(RangeNode(Sym("x")))
    sdql"range(x.z)" should be(RangeNode(FieldNode(Sym("x"), "z")))
  }

  it should "work for int hints" in {
    sdql"@int 2147483647" should be(Const(2147483647))
    sdql"@long 2147483648" should be(Const(2147483648L))
  }

  it should "work for dict hints" in {
    sdql"@phmap {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), PHmap()))
    sdql"@phmap {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), PHmap()))
    sdql"@phmap(100) {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), PHmap(Some(Const(100)))))
    sdql"@phmap(a) {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), PHmap(Some(Sym("a")))))
    sdql"@st {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), SortedDict()))
    sdql"@st {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), SortedDict()))
    sdql"@st(100) {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), SortedDict(Some(Const(100)))))
    sdql"@st(a) {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), SortedDict(Some(Sym("a")))))
    sdql"@smallvecdict(4) {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), SmallVecDict(4)))
    sdql"@smallvecdict(4) {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), SmallVecDict(4)))
    sdql"@smallvecdicts(4) {< foo = 1  > -> 1}" should be(
      DictNode(Seq(RecNode(Seq("foo" -> Const(1.0))) -> Const(1)), SmallVecDicts(4))
    )
    sdql"@smallvecdicts(4) {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), SmallVecDicts(4)))
    sdql"@vec {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), Vec()))
    sdql"@vec {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), Vec()))
    sdql"@vec(1) {0 -> 1}" should be(DictNode(Seq(Const(0) -> Const(1)), Vec(Some(1))))
    sdql"@vec(100) {x -> y}" should be(DictNode(Seq(Sym("x") -> Sym("y")), Vec(Some(100))))
  }

  it should "work for record" in {
    sdql"< foo = 1  >" should be(RecNode(Seq("foo" -> Const(1.0))))
    sdql"< foo = 1, goo  =  hoo  >" should be(RecNode(Seq("foo" -> Const(1.0), "goo" -> Sym("hoo"))))
    sdql"x.name" should be(FieldNode(Sym("x"), "name"))
    sdql"x.name * 2" should be(Mult(FieldNode(Sym("x"), "name"), Const(2.0)))
    sdql"x.name * y.foo" should be(Mult(FieldNode(Sym("x"), "name"), FieldNode(Sym("y"), "foo")))
    sdql"x(1)" should be(Get(Sym("x"), Const(1)))
    sdql"< foo = 1  >(1)" should be(Get(RecNode(Seq("foo" -> Const(1.0))), Const(1)))
    sdql"concat(x, y)" should be(Concat(Sym("x"), Sym("y")))
  }

  it should "work for load & ext" in {
    sdql"ext(`TopN`, x)" should be(External("TopN", Seq(Sym("x"))))
    sdql"""load[{string -> bool}]("foo.csv")""" should be(Load("foo.csv", DictType(StringType(), BoolType)))
    sdql"""load[{string -> bool}]("foo.csv", { 0, 1 })""" should be(
      Load("foo.csv", DictType(StringType(), BoolType), SetNode(Seq(Const(0), Const(1))))
    )
  }

  it should "work for semirings" in {
    sdql"""promote[mxsm](x)""" should be(Promote(TropicalSemiRingType("max_sum"), Sym("x")))
    sdql"""promote[min_sum](x)""" should be(Promote(TropicalSemiRingType("min_sum"), Sym("x")))
    sdql"""promote[enum[int]](x)""" should be(Promote(EnumSemiRingType(IntType), Sym("x")))
    sdql"""promote[nullable[int]](x)""" should be(Promote(NullableSemiRingType(IntType), Sym("x")))
    sdql"""load[{string -> min_prod}]("foo.csv")""" should be(
      Load("foo.csv", DictType(StringType(), TropicalSemiRingType("min_prod")))
    )
  }

  it should "perform desugaring" in {
    sdql"x ^ 0" should be(sdql"1")
    sdql"x ^ 1" should be(sdql"x")
    sdql"x ^ 2" should be(sdql"x * x")
    sdql"x ^ 3" should be(sdql"x * x * x")
    sdql"y * x ^ 2" should be(sdql"y * (x * x)")
  }

  it should "splice constants" in {
    val TRUE     = true
    val FALSE    = false
    val ONE      = 1
    val ONE_HALF = 1.5
    val MAP      = Map(ONE -> TRUE)
    val REC      = RecordValue(Seq("a" -> ONE, "b" -> ONE_HALF))
    val MAP_REC  = Map(REC -> ONE_HALF)
    val STRING   = "foo"
    sdql"$TRUE" should be(sdql"true")
    sdql"$FALSE" should be(sdql"false")
    sdql"$ONE" should be(sdql"1")
    sdql"$ONE_HALF" should be(sdql"1.5")
    sdql"$MAP" should be(sdql"{1 -> true}")
    sdql"$REC" should be(sdql"<a=1,b=1.5>")
    sdql"$MAP_REC" should be(sdql"{ <a=1,b=1.5> -> 1.5 }")
    sdql"$STRING" should be(sdql""" "foo" """)
  }

  it should "parse TPCH" in {
    SourceCode.fromFile("progs/tpch/q1.sdql")
    SourceCode.fromFile("progs/tpch/q2.sdql")
    SourceCode.fromFile("progs/tpch/q3.sdql")
    SourceCode.fromFile("progs/tpch/q4.sdql")
    SourceCode.fromFile("progs/tpch/q5.sdql")
    SourceCode.fromFile("progs/tpch/q6.sdql")
    SourceCode.fromFile("progs/tpch/q7.sdql")
    SourceCode.fromFile("progs/tpch/q8.sdql")
    SourceCode.fromFile("progs/tpch/q9.sdql")
    SourceCode.fromFile("progs/tpch/q10.sdql")
    SourceCode.fromFile("progs/tpch/q11.sdql")
    SourceCode.fromFile("progs/tpch/q12.sdql")
    SourceCode.fromFile("progs/tpch/q13.sdql")
    SourceCode.fromFile("progs/tpch/q14.sdql")
    SourceCode.fromFile("progs/tpch/q15.sdql")
    SourceCode.fromFile("progs/tpch/q16.sdql")
    SourceCode.fromFile("progs/tpch/q17.sdql")
    SourceCode.fromFile("progs/tpch/q18.sdql")
    SourceCode.fromFile("progs/tpch/q19.sdql")
    SourceCode.fromFile("progs/tpch/q20.sdql")
    SourceCode.fromFile("progs/tpch/q21.sdql")
    SourceCode.fromFile("progs/tpch/q22.sdql")
  }

  it should "parse JOB GJ" in {
    SourceCode.fromFile("progs/job/gj/1a.sdql")
    SourceCode.fromFile("progs/job/gj/1b.sdql")
    SourceCode.fromFile("progs/job/gj/1c.sdql")
    SourceCode.fromFile("progs/job/gj/1d.sdql")

    SourceCode.fromFile("progs/job/gj/2a.sdql")
    SourceCode.fromFile("progs/job/gj/2b.sdql")
    SourceCode.fromFile("progs/job/gj/2d.sdql")

    SourceCode.fromFile("progs/job/gj/3a.sdql")
    SourceCode.fromFile("progs/job/gj/3b.sdql")
    SourceCode.fromFile("progs/job/gj/3c.sdql")

    SourceCode.fromFile("progs/job/gj/4a.sdql")
    SourceCode.fromFile("progs/job/gj/4b.sdql")
    SourceCode.fromFile("progs/job/gj/4c.sdql")

    SourceCode.fromFile("progs/job/gj/5c.sdql")

    SourceCode.fromFile("progs/job/gj/6a.sdql")
    SourceCode.fromFile("progs/job/gj/6b.sdql")
    SourceCode.fromFile("progs/job/gj/6c.sdql")
    SourceCode.fromFile("progs/job/gj/6d.sdql")
    SourceCode.fromFile("progs/job/gj/6e.sdql")
    SourceCode.fromFile("progs/job/gj/6f.sdql")

    SourceCode.fromFile("progs/job/gj/7a.sdql")
    SourceCode.fromFile("progs/job/gj/7b.sdql")
    SourceCode.fromFile("progs/job/gj/7c.sdql")

    SourceCode.fromFile("progs/job/gj/8a.sdql")
    SourceCode.fromFile("progs/job/gj/8b.sdql")
    SourceCode.fromFile("progs/job/gj/8c.sdql")
    SourceCode.fromFile("progs/job/gj/8d.sdql")

    SourceCode.fromFile("progs/job/gj/9a.sdql")
    SourceCode.fromFile("progs/job/gj/9b.sdql")
    SourceCode.fromFile("progs/job/gj/9c.sdql")
    SourceCode.fromFile("progs/job/gj/9d.sdql")

    SourceCode.fromFile("progs/job/gj/10a.sdql")
    SourceCode.fromFile("progs/job/gj/10c.sdql")

    SourceCode.fromFile("progs/job/gj/11a.sdql")
    SourceCode.fromFile("progs/job/gj/11b.sdql")
    SourceCode.fromFile("progs/job/gj/11c.sdql")
    SourceCode.fromFile("progs/job/gj/11d.sdql")

    SourceCode.fromFile("progs/job/gj/12a.sdql")
    SourceCode.fromFile("progs/job/gj/12b.sdql")
    SourceCode.fromFile("progs/job/gj/12c.sdql")

    SourceCode.fromFile("progs/job/gj/13a.sdql")
    SourceCode.fromFile("progs/job/gj/13b.sdql")
    SourceCode.fromFile("progs/job/gj/13c.sdql")
    SourceCode.fromFile("progs/job/gj/13d.sdql")

    SourceCode.fromFile("progs/job/gj/14a.sdql")
    SourceCode.fromFile("progs/job/gj/14b.sdql")
    SourceCode.fromFile("progs/job/gj/14c.sdql")

    SourceCode.fromFile("progs/job/gj/15a.sdql")
    SourceCode.fromFile("progs/job/gj/15b.sdql")
    SourceCode.fromFile("progs/job/gj/15c.sdql")
    SourceCode.fromFile("progs/job/gj/15d.sdql")

    SourceCode.fromFile("progs/job/gj/16a.sdql")
    SourceCode.fromFile("progs/job/gj/16b.sdql")
    SourceCode.fromFile("progs/job/gj/16c.sdql")
    SourceCode.fromFile("progs/job/gj/16d.sdql")

    SourceCode.fromFile("progs/job/gj/17a.sdql")
    SourceCode.fromFile("progs/job/gj/17b.sdql")
    SourceCode.fromFile("progs/job/gj/17c.sdql")
    SourceCode.fromFile("progs/job/gj/17d.sdql")
    SourceCode.fromFile("progs/job/gj/17e.sdql")
    SourceCode.fromFile("progs/job/gj/17f.sdql")

    SourceCode.fromFile("progs/job/gj/18a.sdql")
    SourceCode.fromFile("progs/job/gj/18b.sdql")
    SourceCode.fromFile("progs/job/gj/18c.sdql")

    SourceCode.fromFile("progs/job/gj/19a.sdql")
    SourceCode.fromFile("progs/job/gj/19b.sdql")
    SourceCode.fromFile("progs/job/gj/19c.sdql")
    SourceCode.fromFile("progs/job/gj/19d.sdql")

    SourceCode.fromFile("progs/job/gj/20a.sdql")
    SourceCode.fromFile("progs/job/gj/20b.sdql")
    SourceCode.fromFile("progs/job/gj/20c.sdql")

    SourceCode.fromFile("progs/job/gj/21a.sdql")
    SourceCode.fromFile("progs/job/gj/21b.sdql")
    SourceCode.fromFile("progs/job/gj/21c.sdql")

    SourceCode.fromFile("progs/job/gj/22a.sdql")
    SourceCode.fromFile("progs/job/gj/22b.sdql")
    SourceCode.fromFile("progs/job/gj/22c.sdql")
    SourceCode.fromFile("progs/job/gj/22d.sdql")

    SourceCode.fromFile("progs/job/gj/23a.sdql")
    SourceCode.fromFile("progs/job/gj/23b.sdql")
    SourceCode.fromFile("progs/job/gj/23c.sdql")

    SourceCode.fromFile("progs/job/gj/24a.sdql")
    SourceCode.fromFile("progs/job/gj/24b.sdql")

    SourceCode.fromFile("progs/job/gj/25a.sdql")
    SourceCode.fromFile("progs/job/gj/25b.sdql")
    SourceCode.fromFile("progs/job/gj/25c.sdql")

    SourceCode.fromFile("progs/job/gj/26a.sdql")
    SourceCode.fromFile("progs/job/gj/26b.sdql")
    SourceCode.fromFile("progs/job/gj/26c.sdql")

    SourceCode.fromFile("progs/job/gj/27a.sdql")
    SourceCode.fromFile("progs/job/gj/27b.sdql")
    SourceCode.fromFile("progs/job/gj/27c.sdql")

    SourceCode.fromFile("progs/job/gj/28a.sdql")
    SourceCode.fromFile("progs/job/gj/28b.sdql")
    SourceCode.fromFile("progs/job/gj/28c.sdql")

    SourceCode.fromFile("progs/job/gj/29a.sdql")
    SourceCode.fromFile("progs/job/gj/29b.sdql")
    SourceCode.fromFile("progs/job/gj/29c.sdql")

    SourceCode.fromFile("progs/job/gj/30a.sdql")
    SourceCode.fromFile("progs/job/gj/30b.sdql")
    SourceCode.fromFile("progs/job/gj/30c.sdql")

    SourceCode.fromFile("progs/job/gj/31a.sdql")
    SourceCode.fromFile("progs/job/gj/31b.sdql")
    SourceCode.fromFile("progs/job/gj/31c.sdql")

    SourceCode.fromFile("progs/job/gj/32b.sdql")

    SourceCode.fromFile("progs/job/gj/33a.sdql")
    SourceCode.fromFile("progs/job/gj/33b.sdql")
    SourceCode.fromFile("progs/job/gj/33c.sdql")
  }

  it should "parse JOB FJ" in {
    SourceCode.fromFile("progs/job/fj/1a.sdql")
    SourceCode.fromFile("progs/job/fj/1b.sdql")
    SourceCode.fromFile("progs/job/fj/1c.sdql")
    SourceCode.fromFile("progs/job/fj/1d.sdql")

    SourceCode.fromFile("progs/job/fj/2a.sdql")
    SourceCode.fromFile("progs/job/fj/2b.sdql")
    SourceCode.fromFile("progs/job/fj/2d.sdql")

    SourceCode.fromFile("progs/job/fj/3a.sdql")
    SourceCode.fromFile("progs/job/fj/3b.sdql")
    SourceCode.fromFile("progs/job/fj/3c.sdql")

    SourceCode.fromFile("progs/job/fj/4a.sdql")
    SourceCode.fromFile("progs/job/fj/4b.sdql")
    SourceCode.fromFile("progs/job/fj/4c.sdql")

    SourceCode.fromFile("progs/job/fj/5c.sdql")

    SourceCode.fromFile("progs/job/fj/6a.sdql")
    SourceCode.fromFile("progs/job/fj/6b.sdql")
    SourceCode.fromFile("progs/job/fj/6c.sdql")
    SourceCode.fromFile("progs/job/fj/6d.sdql")
    SourceCode.fromFile("progs/job/fj/6e.sdql")
    SourceCode.fromFile("progs/job/fj/6f.sdql")

    SourceCode.fromFile("progs/job/fj/7a.sdql")
    SourceCode.fromFile("progs/job/fj/7b.sdql")
    SourceCode.fromFile("progs/job/fj/7c.sdql")

    SourceCode.fromFile("progs/job/fj/8a.sdql")
    SourceCode.fromFile("progs/job/fj/8b.sdql")
    SourceCode.fromFile("progs/job/fj/8c.sdql")
    SourceCode.fromFile("progs/job/fj/8d.sdql")

    SourceCode.fromFile("progs/job/fj/9a.sdql")
    SourceCode.fromFile("progs/job/fj/9b.sdql")
    SourceCode.fromFile("progs/job/fj/9c.sdql")
    SourceCode.fromFile("progs/job/fj/9d.sdql")

    SourceCode.fromFile("progs/job/fj/10a.sdql")
    SourceCode.fromFile("progs/job/fj/10c.sdql")

    SourceCode.fromFile("progs/job/fj/11a.sdql")
    SourceCode.fromFile("progs/job/fj/11b.sdql")
    SourceCode.fromFile("progs/job/fj/11c.sdql")
    SourceCode.fromFile("progs/job/fj/11d.sdql")

    SourceCode.fromFile("progs/job/fj/12a.sdql")
    SourceCode.fromFile("progs/job/fj/12b.sdql")
    SourceCode.fromFile("progs/job/fj/12c.sdql")

    SourceCode.fromFile("progs/job/fj/13a.sdql")
    SourceCode.fromFile("progs/job/fj/13b.sdql")
    SourceCode.fromFile("progs/job/fj/13c.sdql")
    SourceCode.fromFile("progs/job/fj/13d.sdql")

    SourceCode.fromFile("progs/job/fj/14a.sdql")
    SourceCode.fromFile("progs/job/fj/14b.sdql")
    SourceCode.fromFile("progs/job/fj/14c.sdql")

    SourceCode.fromFile("progs/job/fj/15a.sdql")
    SourceCode.fromFile("progs/job/fj/15b.sdql")
    SourceCode.fromFile("progs/job/fj/15c.sdql")
    SourceCode.fromFile("progs/job/fj/15d.sdql")

    SourceCode.fromFile("progs/job/fj/16a.sdql")
    SourceCode.fromFile("progs/job/fj/16b.sdql")
    SourceCode.fromFile("progs/job/fj/16c.sdql")
    SourceCode.fromFile("progs/job/fj/16d.sdql")

    SourceCode.fromFile("progs/job/fj/17a.sdql")
    SourceCode.fromFile("progs/job/fj/17b.sdql")
    SourceCode.fromFile("progs/job/fj/17c.sdql")
    SourceCode.fromFile("progs/job/fj/17d.sdql")
    SourceCode.fromFile("progs/job/fj/17e.sdql")
    SourceCode.fromFile("progs/job/fj/17f.sdql")

    SourceCode.fromFile("progs/job/fj/18a.sdql")
    SourceCode.fromFile("progs/job/fj/18b.sdql")
    SourceCode.fromFile("progs/job/fj/18c.sdql")

    SourceCode.fromFile("progs/job/fj/19a.sdql")
    SourceCode.fromFile("progs/job/fj/19b.sdql")
    SourceCode.fromFile("progs/job/fj/19c.sdql")
    SourceCode.fromFile("progs/job/fj/19d.sdql")

    SourceCode.fromFile("progs/job/fj/20a.sdql")
    SourceCode.fromFile("progs/job/fj/20b.sdql")
    SourceCode.fromFile("progs/job/fj/20c.sdql")

    SourceCode.fromFile("progs/job/fj/21a.sdql")
    SourceCode.fromFile("progs/job/fj/21b.sdql")
    SourceCode.fromFile("progs/job/fj/21c.sdql")

    SourceCode.fromFile("progs/job/fj/22a.sdql")
    SourceCode.fromFile("progs/job/fj/22b.sdql")
    SourceCode.fromFile("progs/job/fj/22c.sdql")
    SourceCode.fromFile("progs/job/fj/22d.sdql")

    SourceCode.fromFile("progs/job/fj/23a.sdql")
    SourceCode.fromFile("progs/job/fj/23b.sdql")
    SourceCode.fromFile("progs/job/fj/23c.sdql")

    SourceCode.fromFile("progs/job/fj/24a.sdql")
    SourceCode.fromFile("progs/job/fj/24b.sdql")

    SourceCode.fromFile("progs/job/fj/25a.sdql")
    SourceCode.fromFile("progs/job/fj/25b.sdql")
    SourceCode.fromFile("progs/job/fj/25c.sdql")

    SourceCode.fromFile("progs/job/fj/26a.sdql")
    SourceCode.fromFile("progs/job/fj/26b.sdql")
    SourceCode.fromFile("progs/job/fj/26c.sdql")

    SourceCode.fromFile("progs/job/fj/27a.sdql")
    SourceCode.fromFile("progs/job/fj/27b.sdql")
    SourceCode.fromFile("progs/job/fj/27c.sdql")

    SourceCode.fromFile("progs/job/fj/28a.sdql")
    SourceCode.fromFile("progs/job/fj/28b.sdql")
    SourceCode.fromFile("progs/job/fj/28c.sdql")

    SourceCode.fromFile("progs/job/fj/29a.sdql")
    SourceCode.fromFile("progs/job/fj/29b.sdql")
    SourceCode.fromFile("progs/job/fj/29c.sdql")

    SourceCode.fromFile("progs/job/fj/30a.sdql")
    SourceCode.fromFile("progs/job/fj/30b.sdql")
    SourceCode.fromFile("progs/job/fj/30c.sdql")

    SourceCode.fromFile("progs/job/fj/31a.sdql")
    SourceCode.fromFile("progs/job/fj/31b.sdql")
    SourceCode.fromFile("progs/job/fj/31c.sdql")

    SourceCode.fromFile("progs/job/fj/32b.sdql")

    SourceCode.fromFile("progs/job/fj/33a.sdql")
    SourceCode.fromFile("progs/job/fj/33b.sdql")
    SourceCode.fromFile("progs/job/fj/33c.sdql")
  }

  it should "parse LSQB GJ" in {
    SourceCode.fromFile("progs/lsqb/gj/q1.sdql")
    SourceCode.fromFile("progs/lsqb/gj/q2.sdql")
    SourceCode.fromFile("progs/lsqb/gj/q4.sdql")
    SourceCode.fromFile("progs/lsqb/gj/q5.sdql")
  }

  it should "parse LSQB FJ" in {
    SourceCode.fromFile("progs/lsqb/fj/q1.sdql")
    SourceCode.fromFile("progs/lsqb/fj/q2.sdql")
    SourceCode.fromFile("progs/lsqb/fj/q4.sdql")
    SourceCode.fromFile("progs/lsqb/fj/q5.sdql")
  }
}
