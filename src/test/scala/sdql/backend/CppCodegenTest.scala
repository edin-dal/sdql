package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.frontend.SourceCode

class CppCodegenTest extends AnyFlatSpec with ParallelTestExecution {

  it should "compile TPCH Q1" in {
    assertCompiles("progs/tpch/q1.sdql")
  }

  it should "compile TPCH Q2" in {
    assertCompiles("progs/tpch/q2.sdql")
  }

  it should "compile TPCH Q3" in {
    assertCompiles("progs/tpch/q3.sdql")
  }

  it should "compile TPCH Q4" in {
    assertCompiles("progs/tpch/q4.sdql")
  }

  it should "compile TPCH Q5" in {
    assertCompiles("progs/tpch/q5.sdql")
  }

  it should "compile TPCH Q6" in {
    assertCompiles("progs/tpch/q6.sdql")
  }

  it should "compile TPCH Q7" in {
    assertCompiles("progs/tpch/q7.sdql")
  }

// FIXME
//  it should "compile TPCH Q8" in {
//    assertCompiles("progs/tpch/q8.sdql")
//  }

  it should "compile TPCH Q9" in {
    assertCompiles("progs/tpch/q9.sdql")
  }

  it should "compile TPCH Q10" in {
    assertCompiles("progs/tpch/q10.sdql")
  }

// FIXME
//  it should "compile TPCH Q11" in {
//    assertCompiles("progs/tpch/q11.sdql")
//  }

// FIXME
//  it should "compile TPCH Q12" in {
//    assertCompiles("progs/tpch/q12.sdql")
//  }

  it should "compile TPCH Q13" in {
    assertCompiles("progs/tpch/q13.sdql")
  }

  it should "compile TPCH Q14" in {
    assertCompiles("progs/tpch/q14.sdql")
  }

// FIXME
//  it should "compile TPCH Q15" in {
//    assertCompiles("progs/tpch/q15.sdql")
//  }

  it should "compile TPCH Q16" in {
    assertCompiles("progs/tpch/q16.sdql")
  }

  it should "compile TPCH Q17" in {
    assertCompiles("progs/tpch/q17.sdql")
  }

// FIXME
//  it should "compile TPCH Q18" in {
//    assertCompiles("progs/tpch/q18.sdql")
//  }

  it should "compile TPCH Q19" in {
    assertCompiles("progs/tpch/q19.sdql")
  }

  it should "compile TPCH Q20" in {
    assertCompiles("progs/tpch/q20.sdql")
  }

  it should "compile TPCH Q21" in {
    assertCompiles("progs/tpch/q21.sdql")
  }

  it should "compile TPCH Q22" in {
    assertCompiles("progs/tpch/q22.sdql")
  }

  // TODO avoid replacing single quotes in source code
  private def assertCompiles(fileName: String) = assert(compile(fileName) == 0)
  private def compile(fileName: String) = fromSource(CppCodegen(SourceCode.fromFile(fileName).exp))
  private def fromSource(cpp: String) = CppCompilation.inGeneratedDir(Seq("bash", "-c", fixCmd(cpp))).run().exitValue()
  private val fixCmd = cmd _ compose fix
  private def fix(cpp: String) = cpp.replace(singleQuote.toString, s"char(${singleQuote.toInt})")
  private def cmd(cpp: String) = s"${CppCompilation.clangCmd.mkString(" ")} -xc++ -fsyntax-only - <<< '$cpp'"
  private val singleQuote = '\''
}
