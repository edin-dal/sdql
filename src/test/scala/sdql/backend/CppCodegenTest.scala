package sdql
package backend

import org.scalatest.flatspec.AnyFlatSpec
import sdql.frontend.SourceCode

class CppCodegenTest extends AnyFlatSpec {

  // FIXME fix broken tests
  it should "compile TPCH" in {
    assertCompiles("progs/tpch/q1.sdql")
    assertCompiles("progs/tpch/q2.sdql")
    assertCompiles("progs/tpch/q3.sdql")
    assertCompiles("progs/tpch/q4.sdql")
    assertCompiles("progs/tpch/q5.sdql")
    assertCompiles("progs/tpch/q6.sdql")
    assertCompiles("progs/tpch/q7.sdql")
    // assertCompiles("progs/tpch/q8.sdql")
    assertCompiles("progs/tpch/q9.sdql")
    assertCompiles("progs/tpch/q10.sdql")
    // assertCompiles("progs/tpch/q11.sdql")
    // assertCompiles("progs/tpch/q12.sdql")
    assertCompiles("progs/tpch/q13.sdql")
    assertCompiles("progs/tpch/q14.sdql")
    // assertCompiles("progs/tpch/q15.sdql")
    assertCompiles("progs/tpch/q16.sdql")
    assertCompiles("progs/tpch/q17.sdql")
    // assertCompiles("progs/tpch/q18.sdql")
    assertCompiles("progs/tpch/q19.sdql")
    assertCompiles("progs/tpch/q20.sdql")
    assertCompiles("progs/tpch/q21.sdql")
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
