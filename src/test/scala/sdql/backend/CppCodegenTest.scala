package sdql
package backend

import org.scalatest.flatspec.AnyFlatSpec
import sdql.frontend.SourceCode

class CppCodegenTest extends AnyFlatSpec {

  // FIXME fix broken tests
  it should "compile TPCH" in {
    compile("progs/tpch/q1.sdql")
    compile("progs/tpch/q2.sdql")
    compile("progs/tpch/q3.sdql")
    // compile("progs/tpch/q4.sdql")
    compile("progs/tpch/q5.sdql")
    compile("progs/tpch/q6.sdql")
    compile("progs/tpch/q7.sdql")
    // compile("progs/tpch/q8.sdql")
    compile("progs/tpch/q9.sdql")
    compile("progs/tpch/q10.sdql")
    // compile("progs/tpch/q11.sdql")
    // compile("progs/tpch/q12.sdql")
    compile("progs/tpch/q13.sdql")
    compile("progs/tpch/q14.sdql")
    // compile("progs/tpch/q15.sdql")
    compile("progs/tpch/q16.sdql")
    compile("progs/tpch/q17.sdql")
    // compile("progs/tpch/q18.sdql")
    compile("progs/tpch/q19.sdql")
    compile("progs/tpch/q20.sdql")
    compile("progs/tpch/q21.sdql")
    compile("progs/tpch/q22.sdql")
  }

  // TODO don't replace single quotes in source code
  private def compile(fileName: String) = fromSource(CppCodegen(SourceCode.fromFile(fileName).exp))
  private def fromSource(cpp: String) = assert(in_generated(Seq("bash", "-c", fixCmd(cpp))).run().exitValue() == 0)
  private val fixCmd = cmd _ compose fix
  private def fix(cpp: String) = cpp.replace(singleQuote.toString, s"char(${singleQuote.toInt})")
  private def cmd(cpp: String) = s"clang++ -Wno-deprecated-builtins --std c++20 -xc++ -fsyntax-only - <<< '$cpp'"
  private def in_generated(seq: Seq[String]) = sys.process.Process(seq, generatedDir)
  private val generatedDir = new java.io.File("generated")
  private val singleQuote = '\''
}
