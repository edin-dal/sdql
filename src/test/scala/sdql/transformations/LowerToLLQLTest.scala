package sdql.transformations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.frontend.*
import sdql.ir.*

class LowerToLLQLTest extends AnyFlatSpec with Matchers {
  it should "lower sum" in {
    //         sum(<i, _> <- {2}) i
    // let x = sum(<i, _> <- {2}) i in x
    val sum = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), Sym("i"))
    val e   = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) i in x")

    val rewrite = LetBinding(Sym("x"), Initialise(IntType, SumAgg, sum), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }

  it should "lower nested sum" in {
    //                            sum(<j, _> <- {3}) i * j
    //         sum(<i, _> <- {2}) sum(<j, _> <- {3}) i * j in x
    // let x = sum(<i, _> <- {2}) sum(<j, _> <- {3}) i * j in x
    val inner = Sum(Sym("j"), Sym("_"), SetNode(Seq(Const(3))), Mult(Sym("i"), Sym("j")))
    val sum   = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), inner)
    val e     = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) sum(<j, _> <- {3}) i * j in x")

    val rewrite = LetBinding(Sym("x"), Initialise(IntType, SumAgg, sum), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }

  it should "lower nested let sum" in {
    //                                    sum(<j, _> <- {3}) j
    //                            let k = sum(<j, _> <- {3}) j in k
    //         sum(<i, _> <- {2}) let k = sum(<j, _> <- {3}) j in k
    // let x = sum(<i, _> <- {2}) let k = sum(<j, _> <- {3}) j in k in x
    val innerSum = Sum(Sym("j"), Sym("_"), SetNode(Seq(Const(3))), Sym("j"))
    val inner    = LetBinding(Sym("k"), innerSum, Sym("k"))
    val sum      = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), inner)
    val e        = LetBinding(Sym("x"), sum, Sym("x"))
    e should be(sdql"let x = sum(<i, _> <- {2}) let k = sum(<j, _> <- {3}) j in k in x")

    val innerRewrite = LetBinding(Sym("k"), Initialise(IntType, SumAgg, innerSum), Sym("k"))
    LowerToLLQL(inner) should be(innerRewrite)

    val sumRewrite = Sum(Sym("i"), Sym("_"), SetNode(Seq(Const(2))), innerRewrite)
    LowerToLLQL(sum) should be(sumRewrite)

    val rewrite = LetBinding(Sym("x"), Initialise(IntType, SumAgg, sumRewrite), Sym("x"))
    LowerToLLQL(e) should be(rewrite)
  }
}
