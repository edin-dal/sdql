package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.frontend.{Interpolator, SourceCode}
import sdql.ir.Exp

class CppCodegenTest extends AnyFlatSpec with ParallelTestExecution {

  it should "compile constant true" in {
    compilesExp(sdql"true")
  }
  it should "compile constant false" in {
    compilesExp(sdql"false")
  }
  it should "compile constant int" in {
    compilesExp(sdql"42")
  }
  it should "compile constant real" in {
    compilesExp(sdql"42.2")
  }
  it should "compile constant string" in {
    compilesExp(sdql""" "foo" """)
  }
  it should "compile constant date" in {
    compilesExp(sdql"date(19700101)")
  }
  it should "compile constant tuple" in {
    compilesExp(sdql"< a = 1, b = 2 >")
  }
  it should "compile constant map" in {
    compilesExp(sdql"""{ "a" -> 1, "b" -> 2 }""")
  }
  // TODO this should be a unit test for type inference instead
  it should "compile constant map requiring type promotion" in {
    val e = sdql"""{ "a" -> 1, "b" -> 2.0 }"""
    import sdql.analysis.TypeInference
    import sdql.ir.{DictType, RealType, StringType}
    assert(TypeInference(e) == DictType(StringType, RealType))
    compilesExp(e)
  }

  it should "compile TPCH Q1" in {
    compilesFile("progs/tpch/q1.sdql")
  }
  it should "compile TPCH Q2" in {
    compilesFile("progs/tpch/q2.sdql")
  }
  it should "compile TPCH Q3" in {
    compilesFile("progs/tpch/q3.sdql")
  }
  it should "compile TPCH Q4" in {
    compilesFile("progs/tpch/q4.sdql")
  }
  it should "compile TPCH Q5" in {
    compilesFile("progs/tpch/q5.sdql")
  }
  it should "compile TPCH Q6" in {
    compilesFile("progs/tpch/q6.sdql")
  }
  it should "compile TPCH Q7" in {
    compilesFile("progs/tpch/q7.sdql")
  }
// FIXME
//  it should "compile TPCH Q8" in {
//    compilesFile("progs/tpch/q8.sdql")
//  }
  it should "compile TPCH Q9" in {
    compilesFile("progs/tpch/q9.sdql")
  }
  it should "compile TPCH Q10" in {
    compilesFile("progs/tpch/q10.sdql")
  }
// FIXME
//  it should "compile TPCH Q11" in {
//    compilesFile("progs/tpch/q11.sdql")
//  }
// FIXME
//  it should "compile TPCH Q12" in {
//    compilesFile("progs/tpch/q12.sdql")
//  }
  it should "compile TPCH Q13" in {
    compilesFile("progs/tpch/q13.sdql")
  }
  it should "compile TPCH Q14" in {
    compilesFile("progs/tpch/q14.sdql")
  }
// FIXME
//  it should "compile TPCH Q15" in {
//    compilesFile("progs/tpch/q15.sdql")
//  }
  it should "compile TPCH Q16" in {
    compilesFile("progs/tpch/q16.sdql")
  }
  it should "compile TPCH Q17" in {
    compilesFile("progs/tpch/q17.sdql")
  }
// FIXME
//  it should "compile TPCH Q18" in {
//    compilesFile("progs/tpch/q18.sdql")
//  }
  it should "compile TPCH Q19" in {
    compilesFile("progs/tpch/q19.sdql")
  }
  it should "compile TPCH Q20" in {
    compilesFile("progs/tpch/q20.sdql")
  }
  it should "compile TPCH Q21" in {
    compilesFile("progs/tpch/q21.sdql")
  }
  it should "compile TPCH Q22" in {
    compilesFile("progs/tpch/q22.sdql")
  }

  private def compilesFile(path: String) = compilesExp(SourceCode.fromFile(path).exp)
  private def compilesExp(e: Exp) = assert(compile(e) == 0)
  private def compile(exp: Exp) = fromCpp(CppCodegen(exp))
  private def fromCpp(cpp: String) = CppCompilation.inGeneratedDir(Seq("bash", "-c", cmd(escape(cpp)))).run().exitValue()
  private def cmd(cpp: String) = s"${CppCompilation.clangCmd.mkString(" ")} -xc++ -fsyntax-only - <<< '$cpp'"
  // TODO avoid escaping single quotes in source code
  private def escape(cpp: String) = cpp.replace(singleQuote.toString, s"char(${singleQuote.toInt})")
  private val singleQuote = '\''
}
