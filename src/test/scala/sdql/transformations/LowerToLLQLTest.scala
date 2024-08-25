package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.frontend.*
import sdql.ir.*

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
    val rewrite     = LetBinding(Sym("x"), Initialise(IntType, SumAgg, sumRewrite), Sym("x"))
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
    val rewrite      = LetBinding(Sym("x"), Initialise(IntType, SumAgg, sumRewrite), Sym("x"))
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
      LetBinding(Sym("k"), Initialise(IntType, SumAgg, innerSumRewrite), Update(Sym("k"), SumAgg, Sym("x")))
    val sumRewrite = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), innerRewrite)
    val rewrite    = LetBinding(Sym("x"), Initialise(IntType, SumAgg, sumRewrite), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }
}
