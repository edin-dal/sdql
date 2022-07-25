package sdql
package frontend

import ir._
import org.scalatest._
import Matchers._

class ParserTest extends FlatSpec {

  "Parser" should "work for constant terms" in {
    sdql"true" should be (Const(true))
    sdql"false" should be (Const(false))
    sdql"52" should be (Const(52))
    sdql"-52" should be (Const(-52))
    sdql"52.1" should be (Const(52.1))
    sdql""" "foo" """ should be (Const("foo"))
    sdql"dense_int(999, -1)" should be (Const(DenseInt(999, -1)))
    sdql"dense_int(12, 255)" should be (Const(DenseInt(12, 255)))
    sdql"date(19890713)" should be (Const(DateValue(19890713)))
    assertThrows[Exception] {
      sdql"`foo`"
    }
  }

  it should "work for if then else" in {
    sdql"if true then 0 else 1" should be (IfThenElse(Const(true), Const(0), Const(1)))
    sdql"if (true) then (0) else (1)" should be (IfThenElse(Const(true), Const(0), Const(1)))
    sdql"if (!true) then (0) else (1)" should be (IfThenElse(Not(Const(true)), Const(0), Const(1)))
  }

  it should "work for letbinding" in {
    sdql"let x = 1 in 2" should be (LetBinding(Sym("x"), Const(1), Const(2)))
    sdql"let    x  =    (1) in    2" should be (LetBinding(Sym("x"), Const(1), Const(2)))
    sdql"let x_1 = 1 in 2" should be (LetBinding(Sym("x_1"), Const(1), Const(2)))
    sdql"""let X = 1 in 
           2""" should be (LetBinding(Sym("X"), Const(1), Const(2)))
    sdql"x__2" should be (Sym("x__2"))
    sdql"X__2" should be (Sym("X__2"))
    assertThrows[Exception] {
      sdql"let x = 1 (2)"
    }
  }

  it should "work for sum" in {
    sdql"sum(<k,v> <- X) v" should be (Sum(Sym("k"), Sym("v"), Sym("X"), Sym("v")))
    sdql"sum(<k,v> <- X) {k -> v}" should be (Sum(Sym("k"), Sym("v"), Sym("X"), SingleDict(Sym("k"), Sym("v"))))
  }

  it should "handle comments" in {
    val term = LetBinding(Sym("x"), Sym("y"), Sym("z"))
    sdql"/* comment for let */ let x = y in z" should be (term)
    sdql"let x = y in /* comment for let */ z" should be (term)
    sdql"let x = y in z /* cc */" should be (term)
    sdql"""// comment before
  let x = y in // comment middle 1
  // comment middle 2
  z
  """ should be (term)
  sdql"""/* comment before */
  let x /* comment inside */ = /* comment inside */ y in 
  /* comment middle */
  z
  """ should be (term)
  }

  it should "work for arith" in {
    sdql"2 * 3" should be (Mult(Const(2.0), Const(3.0)))
    sdql"2 + 3" should be (Add(Const(2.0), Const(3.0)))
    sdql"2 / 3" should be (Mult(Const(2.0), ExternalFunctions.Inv(Const(3.0))))
    sdql"2 - 3" should be (Add(Const(2.0), Neg(Const(3.0))))
    sdql"2 + 1 * 3" should be (Add(Const(2.0), Mult(Const(1.0), Const(3.0))))
    sdql"2 * 1 + 3" should be (Add(Mult(Const(2.0), Const(1.0)), Const(3.0)))
    sdql"(2 * 1) + 3" should be (Add(Mult(Const(2.0), Const(1.0)), Const(3.0)))
    sdql"-x + y" should be (Add(Neg(Sym("x")), Sym("y")))
    sdql"-x * y" should be (Mult(Neg(Sym("x")), Sym("y")))
    sdql"-(x + y)" should be (Neg(Add(Sym("x"), Sym("y"))))
    sdql"2 < 3" should be (Cmp(Const(2.0), Const(3.0), "<"))
    // sdql"2 > 3" should be (Cmp(Const(2.0), Const(3.0), ">")) // FIXME
    sdql"2 < 3 * 1" should be (Cmp(Const(2.0), Mult(Const(3.0), Const(1.0)), "<"))
    sdql"2 < (3 * 1)" should be (Cmp(Const(2.0), Mult(Const(3.0), Const(1.0)), "<"))
    sdql"2 * 3" should be (Mult(Const(2.0), Const(3.0)))
    // sdql"2 ^ 3" should be (Pow(Const(2.0), Const(3.0)))
    // sdql"2 ^ log(3)" should be (Pow(Const(2.0), Log(Const(3.0))))
    sdql"2 && 3" should be (And(Const(2.0), Const(3.0)))
    sdql"2 || 3" should be (Or(Const(2.0), Const(3.0)))
    sdql"!2" should be (Not(Const(2.0)))
  }

  it should "work for set & dict" in {
    sdql"{}" should be (SetNode(Seq()))
    // sdql"{x}" should be (SetNode(Seq(Sym("x"))))
    // sdql"{  x   , y   }" should be (SetNode(Seq(Sym("x"), Sym("y"))))
    sdql"{x  ->  y}" should be (DictNode(Seq(Sym("x") -> Sym("y"))))
    sdql"{x.z  ->  y}" should be (DictNode(Seq(FieldNode(Sym("x"), "z") -> Sym("y"))))
    sdql"{x  ->  y, z -> 1 }" should be (DictNode(Seq(Sym("x") -> Sym("y"), Sym("z") -> Const(1.0))))
    sdql"x(y)" should be (Get(Sym("x"), Sym("y")))
    sdql"x(y)(z)" should be (Get(Get(Sym("x"), Sym("y")), Sym("z")))
    sdql"x(   y)" should be (Get(Sym("x"), Sym("y")))
    sdql"x(y)  " should be (Get(Sym("x"), Sym("y")))
    sdql"range(3)" should be (RangeNode(3))
  }

  it should "work for record" in {
    sdql"< foo = 1  >" should be (RecNode(Seq("foo" -> Const(1.0))))
    sdql"< foo = 1, goo  =  hoo  >" should be (RecNode(Seq("foo" -> Const(1.0), "goo" -> Sym("hoo"))))
    sdql"x.name" should be (FieldNode(Sym("x"), "name"))
    sdql"x.name * 2" should be (Mult(FieldNode(Sym("x"), "name"), Const(2.0)))
    sdql"x.name * y.foo" should be (Mult(FieldNode(Sym("x"), "name"), FieldNode(Sym("y"), "foo")))
    sdql"x._1" should be (Fst(Sym("x")))
    sdql"concat(x, y)" should be (Concat(Sym("x"), Sym("y")))
  }

  it should "work for load & ext" in {
    sdql"ext(`TopN`, x)" should be (External("TopN", Seq(Sym("x"))))
    sdql"""load[{string -> bool}]("foo.csv")""" should be (Load("foo.csv",DictType(StringType,BoolType)))
    sdql"""load[{<a:dense_int,b:double> -> int}]("foo.csv")""" should be (Load("foo.csv",
      DictType(RecordType(Seq(Attribute("a", DenseIntType(-1)), Attribute("b", RealType))),IntType)))
  }

  it should "work for semirings" in {
    sdql"""promote[mxsm](x)""" should be (Promote(TropicalSemiRingType("max_sum"), Sym("x")))
    sdql"""promote[min_sum](x)""" should be (Promote(TropicalSemiRingType("min_sum"), Sym("x")))
    sdql"""promote[enum[int]](x)""" should be (Promote(EnumSemiRingType(IntType), Sym("x")))
    sdql"""load[{string -> min_prod}]("foo.csv")""" should be (Load("foo.csv",DictType(StringType,TropicalSemiRingType("min_prod"))))
  }

  it should "perform desugaring" in {
    sdql"x ^ 2" should be (sdql"x * x")
    sdql"y * x ^ 2" should be (sdql"y * (x * x)")
  }

  it should "splice constants" in {
    val TRUE = true
    val FALSE = false
    val ONE = 1
    val ONE_HALF = 1.5
    val MAP = Map(ONE -> TRUE)
    val REC = RecordValue(Seq("a" -> ONE, "b" -> ONE_HALF))
    val MAP_REC = Map(REC -> ONE_HALF)
    val STRING = "foo"
    sdql"$TRUE" should be (sdql"true")
    sdql"$FALSE" should be (sdql"false")
    sdql"$ONE" should be (sdql"1")
    sdql"$ONE_HALF" should be (sdql"1.5")
    sdql"$MAP" should be (sdql"{1 -> true}")
    sdql"$REC" should be (sdql"<a=1,b=1.5>")
    sdql"$MAP_REC" should be (sdql"{ <a=1,b=1.5> -> 1.5 }")
    sdql"$STRING" should be (sdql""" "foo" """)
  }

  it should "parse TPCH" in {
    SourceCode.fromFile("progs/tpch/q1.sdql")
    SourceCode.fromFile("progs/tpch/q6.sdql")
  }
}
