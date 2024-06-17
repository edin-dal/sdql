package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.backend.CppCompilation.compile
import sdql.frontend.SourceCode

import java.nio.file.Path

// TODO Q2, Q7, Q8, Q11, Q17, Q20 have empty/zero output - craft better test datasets

// run these tests on dbgen TPCH datasets with scaling factor 0.01
class CppCompilationTest extends AnyFlatSpec with ParallelTestExecution {

  it should "compile and run TPCH Q1" in {
    assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/q1.result")
  }

  it should "compile and run TPCH Q2" in {
    assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/q2.result")
  }

  it should "compile and run TPCH Q3" in {
    assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/q3.result")
  }

  it should "compile and run TPCH Q4" in {
    assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/q4.result")
  }

  it should "compile and run TPCH Q5" in {
    assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/q5.result")
  }

  it should "compile and run TPCH Q6" in {
    assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/q6.result")
  }

  it should "compile and run TPCH Q7" in {
    assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/q7.result")
  }

  it should "compile and run TPCH Q8" in {
    assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/q8.result")
  }

  it should "compile and run TPCH Q9" in {
    assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/q9.result")
  }

  it should "compile and run TPCH Q10" in {
    assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/q10.result")
  }

  it should "compile and run TPCH Q11" in {
    assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/q11.result")
  }

  it should "compile and run TPCH Q12" in {
     assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/q12.result")
  }

  it should "compile and run TPCH Q13" in {
    assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/q13.result")
  }

  it should "compile and run TPCH Q14" in {
    assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/q14.result")
  }

  it should "compile and run TPCH Q15" in {
    assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/q15.result")
  }

  it should "compile and run TPCH Q16" in {
    assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/q16.result")
  }

  it should "compile and run TPCH Q17" in {
    assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/q17.result")
  }

  it should "compile and run TPCH Q18" in {
    assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/q18.result")
  }

  it should "compile and run TPCH Q19" in {
    assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/q19.result")
  }

  it should "compile and run TPCH Q20" in {
    assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/q20.result")
  }

  it should "compile and run TPCH Q21" in {
    assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/q21.result")
  }

  it should "compile and run TPCH Q22" in {
    assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/q22.result")
  }

  private def assertOutputs(sdqlPath: String, outPath: String) = {
    val actual = compileAndRunFile(sdqlPath)
    val source = scala.io.Source.fromFile(outPath)
    val expected = try source.mkString finally source.close()
    assert(actual.trim == expected.trim)
  }
  private def compileAndRunFile(path: String) = compile(Path.of(path), CppCodegen(SourceCode.fromFile(path).exp))
}
