package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.backend.CppCompilation.compile
import sdql.frontend.SourceCode

class CppCompileTest extends AnyFlatSpec with ParallelTestExecution {

// uncomment and run these tests on dbgen TPCH datasets with scaling factor 1 in src/test/tpch/data/SF_1/
// also run this test separately from the others below - there's some concurrency bug when patching paths
//  it should "compile and run TPCH SF=1" in {
//    assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/results/SF_1/q1.result", sf="1")
//    assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/results/SF_1/q2.result", sf="1")
//    assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/results/SF_1/q3.result", sf="1")
//    assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/results/SF_1/q4.result", sf="1")
//    assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/results/SF_1/q5.result", sf="1")
//    assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/results/SF_1/q6.result", sf="1")
//    assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/results/SF_1/q7.result", sf="1")
//    assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/results/SF_1/q8.result", sf="1")
//    assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/results/SF_1/q9.result", sf="1")
//    assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/results/SF_1/q10.result", sf="1")
//    assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/results/SF_1/q11.result", sf="1")
//    assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/results/SF_1/q12.result", sf="1")
//    assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/results/SF_1/q13.result", sf="1")
//    assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/results/SF_1/q14.result", sf="1")
//    assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/results/SF_1/q15.result", sf="1")
//    assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/results/SF_1/q16.result", sf="1")
//    assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/results/SF_1/q17.result", sf="1")
//    assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/results/SF_1/q18.result", sf="1")
//    assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/results/SF_1/q19.result", sf="1")
//    assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/results/SF_1/q20.result", sf="1")
//    assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/results/SF_1/q21.result", sf="1")
//    assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/results/SF_1/q22.result", sf="1")
//  }

  it should "compile and run TPCH Q1 SF=0.01" in {
    assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/results/SF_0.01/q1.result", sf="0.01")
  }

  it should "compile and run TPCH Q2 SF=0.01" in {
    assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/results/SF_0.01/q2.result", sf="0.01")
  }

  it should "compile and run TPCH Q3 SF=0.01" in {
    assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/results/SF_0.01/q3.result", sf="0.01")
  }

  it should "compile and run TPCH Q4 SF=0.01" in {
    assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/results/SF_0.01/q4.result", sf="0.01")
  }

  it should "compile and run TPCH Q5 SF=0.01" in {
    assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/results/SF_0.01/q5.result", sf="0.01")
  }

  it should "compile and run TPCH Q6 SF=0.01" in {
    assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/results/SF_0.01/q6.result", sf="0.01")
  }

  it should "compile and run TPCH Q7 SF=0.01" in {
    assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/results/SF_0.01/q7.result", sf="0.01")
  }

  it should "compile and run TPCH Q8 SF=0.01" in {
    assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/results/SF_0.01/q8.result", sf="0.01")
  }

  it should "compile and run TPCH Q9 SF=0.01" in {
    assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/results/SF_0.01/q9.result", sf="0.01")
  }

  it should "compile and run TPCH Q10 SF=0.01" in {
    assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/results/SF_0.01/q10.result", sf="0.01")
  }

  it should "compile and run TPCH Q11 SF=0.01" in {
    assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/results/SF_0.01/q11.result", sf="0.01")
  }

  it should "compile and run TPCH Q12 SF=0.01" in {
     assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/results/SF_0.01/q12.result", sf="0.01")
  }

  it should "compile and run TPCH Q13 SF=0.01" in {
    assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/results/SF_0.01/q13.result", sf="0.01")
  }

  it should "compile and run TPCH Q14 SF=0.01" in {
    assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/results/SF_0.01/q14.result", sf="0.01")
  }

  it should "compile and run TPCH Q15 SF=0.01" in {
    assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/results/SF_0.01/q15.result", sf="0.01")
  }

  it should "compile and run TPCH Q16 SF=0.01" in {
    assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/results/SF_0.01/q16.result", sf="0.01")
  }

  it should "compile and run TPCH Q17 SF=0.01" in {
    assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/results/SF_0.01/q17.result", sf="0.01")
  }

  it should "compile and run TPCH Q18 SF=0.01" in {
    assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/results/SF_0.01/q18.result", sf="0.01")
  }

  it should "compile and run TPCH Q19 SF=0.01" in {
    assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/results/SF_0.01/q19.result", sf="0.01")
  }

  it should "compile and run TPCH Q20 SF=0.01" in {
    assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/results/SF_0.01/q20.result", sf="0.01")
  }

  it should "compile and run TPCH Q21 SF=0.01" in {
    assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/results/SF_0.01/q21.result", sf="0.01")
  }

  it should "compile and run TPCH Q22 SF=0.01" in {
    assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/results/SF_0.01/q22.result", sf="0.01")
  }

  private def assertOutputs(sdqlPath: String, outPath: String, sf: String) = {
    val source_code = SourceCode.fromFile(sdqlPath, patch(sf))
    val cpp = CppCodegen(source_code.exp)
    val actual = compile(sdqlPath, cpp)
    val source = scala.io.Source.fromFile(outPath)
    val expected = try source.mkString finally source.close()
    assert(actual.trim == expected.trim)
  }
  private def patch(sf: String)(s:String) = s.replace("datasets/tpch/", s"src/test/tpch/data/SF_$sf/")
}
