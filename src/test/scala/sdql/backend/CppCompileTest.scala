package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.backend.CppCompile.compile
import sdql.frontend.SourceCode


class CppCompileTestTPCH0_01 extends AnyFlatSpec with ParallelTestExecution {

  it should "compile and run TPCH Q1 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/results/SF_0.01/q1.result", sf="0.01")
  }

  it should "compile and run TPCH Q2 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/results/SF_0.01/q2.result", sf="0.01")
  }

  it should "compile and run TPCH Q3 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/results/SF_0.01/q3.result", sf="0.01")
  }

  it should "compile and run TPCH Q4 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/results/SF_0.01/q4.result", sf="0.01")
  }

  it should "compile and run TPCH Q5 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/results/SF_0.01/q5.result", sf="0.01")
  }

  it should "compile and run TPCH Q6 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/results/SF_0.01/q6.result", sf="0.01")
  }

  it should "compile and run TPCH Q7 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/results/SF_0.01/q7.result", sf="0.01")
  }

  it should "compile and run TPCH Q8 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/results/SF_0.01/q8.result", sf="0.01")
  }

  it should "compile and run TPCH Q9 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/results/SF_0.01/q9.result", sf="0.01")
  }

  it should "compile and run TPCH Q10 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/results/SF_0.01/q10.result", sf="0.01")
  }

  it should "compile and run TPCH Q11 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/results/SF_0.01/q11.result", sf="0.01")
  }

  it should "compile and run TPCH Q12 SF=0.01" in {
     Helpers.assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/results/SF_0.01/q12.result", sf="0.01")
  }

  it should "compile and run TPCH Q13 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/results/SF_0.01/q13.result", sf="0.01")
  }

  it should "compile and run TPCH Q14 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/results/SF_0.01/q14.result", sf="0.01")
  }

  it should "compile and run TPCH Q15 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/results/SF_0.01/q15.result", sf="0.01")
  }

  it should "compile and run TPCH Q16 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/results/SF_0.01/q16.result", sf="0.01")
  }

  it should "compile and run TPCH Q17 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/results/SF_0.01/q17.result", sf="0.01")
  }

  it should "compile and run TPCH Q18 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/results/SF_0.01/q18.result", sf="0.01")
  }

  it should "compile and run TPCH Q19 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/results/SF_0.01/q19.result", sf="0.01")
  }

  it should "compile and run TPCH Q20 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/results/SF_0.01/q20.result", sf="0.01")
  }

  it should "compile and run TPCH Q21 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/results/SF_0.01/q21.result", sf="0.01")
  }

  it should "compile and run TPCH Q22 SF=0.01" in {
    Helpers.assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/results/SF_0.01/q22.result", sf="0.01")
  }
}

// uncomment and run these tests on dbgen TPCH datasets with scaling factor 1 in src/test/tpch/data/SF_1/
// also run this test separately from the SF=0.01 ones above - there's a concurrency bug when patching paths
// note: DON'T run them all in parallel - unless you have ~64GB of ram
//class CppCompileTestTPCH1 extends AnyFlatSpec with ParallelTestExecution {
//
//  it should "compile and run TPCH Q1-2 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/results/SF_1/q1.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/results/SF_1/q2.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q3-4 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/results/SF_1/q3.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/results/SF_1/q4.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q5-6 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/results/SF_1/q5.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/results/SF_1/q6.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q7-8 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/results/SF_1/q7.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/results/SF_1/q8.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q9-10 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/results/SF_1/q9.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/results/SF_1/q10.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q11-12 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/results/SF_1/q11.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/results/SF_1/q12.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q13-14 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/results/SF_1/q13.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/results/SF_1/q14.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q15-16 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/results/SF_1/q15.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/results/SF_1/q16.result", sf = "1")
//
//  }
//
//  it should "compile and run TPCH 17-18 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/results/SF_1/q17.result", sf = "1")
//    Helpers.assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/results/SF_1/q18.result", sf = "1")
//  }
//
//  it should "compile and run TPCH Q19-20 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/results/SF_1/q19.result", sf="1")
//    Helpers.assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/results/SF_1/q20.result", sf="1")
//  }
//
//  it should "compile and run TPCH Q21-22 SF=1" in {
//    Helpers.assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/results/SF_1/q21.result", sf="1")
//    Helpers.assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/results/SF_1/q22.result", sf="1")
//  }
//}

// uncomment and run these tests on pre-processed datasets in datasets/job
// manually load balanced - batched together to have roughly same times
// note: DON'T run them all in parallel - unless you have ~64GB of ram
//class CppCompileTestJOB extends AnyFlatSpec with ParallelTestExecution {
//
//  it should "compile and run JOB 1-5 & 8-15" in {
//    // 1-5
//    Helpers.assertOutputs("progs/job/gj/1a.sdql", "src/test/job/gj/results/1a.result")
//    Helpers.assertOutputs("progs/job/gj/1b.sdql", "src/test/job/gj/results/1b.result")
//    Helpers.assertOutputs("progs/job/gj/1c.sdql", "src/test/job/gj/results/1c.result")
//    Helpers.assertOutputs("progs/job/gj/1d.sdql", "src/test/job/gj/results/1d.result")
//
//    Helpers.assertOutputs("progs/job/gj/2a.sdql", "src/test/job/gj/results/2a.result")
//    Helpers.assertOutputs("progs/job/gj/2b.sdql", "src/test/job/gj/results/2b.result")
//    Helpers.assertOutputs("progs/job/gj/2d.sdql", "src/test/job/gj/results/2d.result")
//
//    Helpers.assertOutputs("progs/job/gj/3a.sdql", "src/test/job/gj/results/3a.result")
//    Helpers.assertOutputs("progs/job/gj/3b.sdql", "src/test/job/gj/results/3b.result")
//    Helpers.assertOutputs("progs/job/gj/3c.sdql", "src/test/job/gj/results/3c.result")
//
//    Helpers.assertOutputs("progs/job/gj/4a.sdql", "src/test/job/gj/results/4a.result")
//    Helpers.assertOutputs("progs/job/gj/4b.sdql", "src/test/job/gj/results/4b.result")
//    Helpers.assertOutputs("progs/job/gj/4c.sdql", "src/test/job/gj/results/4c.result")
//
//    Helpers.assertOutputs("progs/job/gj/5c.sdql", "src/test/job/gj/results/5c.result")
//
//    // 8-15
//    Helpers.assertOutputs("progs/job/gj/8a.sdql", "src/test/job/gj/results/8a.result")
//    Helpers.assertOutputs("progs/job/gj/8b.sdql", "src/test/job/gj/results/8b.result")
//    Helpers.assertOutputs("progs/job/gj/8c.sdql", "src/test/job/gj/results/8c.result")
//    Helpers.assertOutputs("progs/job/gj/8d.sdql", "src/test/job/gj/results/8d.result")
//
//    Helpers.assertOutputs("progs/job/gj/9a.sdql", "src/test/job/gj/results/9a.result")
//    Helpers.assertOutputs("progs/job/gj/9b.sdql", "src/test/job/gj/results/9b.result")
//    Helpers.assertOutputs("progs/job/gj/9c.sdql", "src/test/job/gj/results/9c.result")
//    Helpers.assertOutputs("progs/job/gj/9d.sdql", "src/test/job/gj/results/9d.result")
//
//    Helpers.assertOutputs("progs/job/gj/10a.sdql", "src/test/job/gj/results/10a.result")
//    Helpers.assertOutputs("progs/job/gj/10c.sdql", "src/test/job/gj/results/10c.result")
//
//    Helpers.assertOutputs("progs/job/gj/11a.sdql", "src/test/job/gj/results/11a.result")
//    Helpers.assertOutputs("progs/job/gj/11b.sdql", "src/test/job/gj/results/11b.result")
//    Helpers.assertOutputs("progs/job/gj/11c.sdql", "src/test/job/gj/results/11c.result")
//    Helpers.assertOutputs("progs/job/gj/11d.sdql", "src/test/job/gj/results/11d.result")
//
//    Helpers.assertOutputs("progs/job/gj/12a.sdql", "src/test/job/gj/results/12a.result")
//    Helpers.assertOutputs("progs/job/gj/12b.sdql", "src/test/job/gj/results/12b.result")
//    Helpers.assertOutputs("progs/job/gj/12c.sdql", "src/test/job/gj/results/12c.result")
//
//    Helpers.assertOutputs("progs/job/gj/13b.sdql", "src/test/job/gj/results/13b.result")
//    Helpers.assertOutputs("progs/job/gj/13c.sdql", "src/test/job/gj/results/13c.result")
//
//    Helpers.assertOutputs("progs/job/gj/14a.sdql", "src/test/job/gj/results/14a.result")
//    Helpers.assertOutputs("progs/job/gj/14b.sdql", "src/test/job/gj/results/14b.result")
//    Helpers.assertOutputs("progs/job/gj/14c.sdql", "src/test/job/gj/results/14c.result")
//
//    Helpers.assertOutputs("progs/job/gj/15a.sdql", "src/test/job/gj/results/15a.result")
//    Helpers.assertOutputs("progs/job/gj/15b.sdql", "src/test/job/gj/results/15b.result")
//    Helpers.assertOutputs("progs/job/gj/15c.sdql", "src/test/job/gj/results/15c.result")
//    Helpers.assertOutputs("progs/job/gj/15d.sdql", "src/test/job/gj/results/15d.result")
//  }
//
//  it should "compile and run JOB 6-7" in {
//    Helpers.assertOutputs("progs/job/gj/6a.sdql", "src/test/job/gj/results/6a.result")
//    Helpers.assertOutputs("progs/job/gj/6b.sdql", "src/test/job/gj/results/6b.result")
//    Helpers.assertOutputs("progs/job/gj/6c.sdql", "src/test/job/gj/results/6c.result")
//    Helpers.assertOutputs("progs/job/gj/6d.sdql", "src/test/job/gj/results/6d.result")
//    Helpers.assertOutputs("progs/job/gj/6e.sdql", "src/test/job/gj/results/6e.result")
//    Helpers.assertOutputs("progs/job/gj/6f.sdql", "src/test/job/gj/results/6f.result")
//
//    Helpers.assertOutputs("progs/job/gj/7a.sdql", "src/test/job/gj/results/7a.result")
//    Helpers.assertOutputs("progs/job/gj/7b.sdql", "src/test/job/gj/results/7b.result")
//    Helpers.assertOutputs("progs/job/gj/7c.sdql", "src/test/job/gj/results/7c.result")
//  }
//
//  it should "compile and run JOB 16-17" in {
//    Helpers.assertOutputs("progs/job/gj/16a.sdql", "src/test/job/gj/results/16a.result")
//    Helpers.assertOutputs("progs/job/gj/16b.sdql", "src/test/job/gj/results/16b.result")
//    Helpers.assertOutputs("progs/job/gj/16c.sdql", "src/test/job/gj/results/16c.result")
//    Helpers.assertOutputs("progs/job/gj/16d.sdql", "src/test/job/gj/results/16d.result")
//
//    Helpers.assertOutputs("progs/job/gj/17a.sdql", "src/test/job/gj/results/17a.result")
//    Helpers.assertOutputs("progs/job/gj/17b.sdql", "src/test/job/gj/results/17b.result")
//    Helpers.assertOutputs("progs/job/gj/17c.sdql", "src/test/job/gj/results/17c.result")
//    Helpers.assertOutputs("progs/job/gj/17d.sdql", "src/test/job/gj/results/17d.result")
//    Helpers.assertOutputs("progs/job/gj/17e.sdql", "src/test/job/gj/results/17e.result")
//    Helpers.assertOutputs("progs/job/gj/17f.sdql", "src/test/job/gj/results/17f.result")
//  }
//
//  it should "compile and run JOB 18-20 & 26" in {
//    // 18-20
//    Helpers.assertOutputs("progs/job/gj/18a.sdql", "src/test/job/gj/results/18a.result")
//    Helpers.assertOutputs("progs/job/gj/18b.sdql", "src/test/job/gj/results/18b.result")
//    Helpers.assertOutputs("progs/job/gj/18c.sdql", "src/test/job/gj/results/18c.result")
//
//    Helpers.assertOutputs("progs/job/gj/19a.sdql", "src/test/job/gj/results/19a.result")
//    Helpers.assertOutputs("progs/job/gj/19b.sdql", "src/test/job/gj/results/19b.result")
//    Helpers.assertOutputs("progs/job/gj/19c.sdql", "src/test/job/gj/results/19c.result")
//    Helpers.assertOutputs("progs/job/gj/19d.sdql", "src/test/job/gj/results/19d.result")
//
//    Helpers.assertOutputs("progs/job/gj/20a.sdql", "src/test/job/gj/results/20a.result")
//    Helpers.assertOutputs("progs/job/gj/20b.sdql", "src/test/job/gj/results/20b.result")
//    Helpers.assertOutputs("progs/job/gj/20c.sdql", "src/test/job/gj/results/20c.result")
//
//    // 26
//    Helpers.assertOutputs("progs/job/gj/26a.sdql", "src/test/job/gj/results/26a.result")
//    Helpers.assertOutputs("progs/job/gj/26b.sdql", "src/test/job/gj/results/26b.result")
//    Helpers.assertOutputs("progs/job/gj/26c.sdql", "src/test/job/gj/results/26c.result")
//  }
//
//  it should "compile and run JOB 21-25 & 27-33" in {
//    // 21-25
//    Helpers.assertOutputs("progs/job/gj/21a.sdql", "src/test/job/gj/results/21a.result")
//    Helpers.assertOutputs("progs/job/gj/21b.sdql", "src/test/job/gj/results/21b.result")
//    Helpers.assertOutputs("progs/job/gj/21c.sdql", "src/test/job/gj/results/21c.result")
//
//    Helpers.assertOutputs("progs/job/gj/22a.sdql", "src/test/job/gj/results/22a.result")
//    Helpers.assertOutputs("progs/job/gj/22b.sdql", "src/test/job/gj/results/22b.result")
//    Helpers.assertOutputs("progs/job/gj/22c.sdql", "src/test/job/gj/results/22c.result")
//    Helpers.assertOutputs("progs/job/gj/22d.sdql", "src/test/job/gj/results/22d.result")
//
//    Helpers.assertOutputs("progs/job/gj/23a.sdql", "src/test/job/gj/results/23a.result")
//    Helpers.assertOutputs("progs/job/gj/23b.sdql", "src/test/job/gj/results/23b.result")
//    Helpers.assertOutputs("progs/job/gj/23c.sdql", "src/test/job/gj/results/23c.result")
//
//    Helpers.assertOutputs("progs/job/gj/24a.sdql", "src/test/job/gj/results/24a.result")
//    Helpers.assertOutputs("progs/job/gj/24b.sdql", "src/test/job/gj/results/24b.result")
//
//    Helpers.assertOutputs("progs/job/gj/25a.sdql", "src/test/job/gj/results/25a.result")
//    Helpers.assertOutputs("progs/job/gj/25b.sdql", "src/test/job/gj/results/25b.result")
//    Helpers.assertOutputs("progs/job/gj/25c.sdql", "src/test/job/gj/results/25c.result")
//
//    // 27-33
//    Helpers.assertOutputs("progs/job/gj/27a.sdql", "src/test/job/gj/results/27a.result")
//    Helpers.assertOutputs("progs/job/gj/27b.sdql", "src/test/job/gj/results/27b.result")
//    Helpers.assertOutputs("progs/job/gj/27c.sdql", "src/test/job/gj/results/27c.result")
//
//    Helpers.assertOutputs("progs/job/gj/28a.sdql", "src/test/job/gj/results/28a.result")
//    Helpers.assertOutputs("progs/job/gj/28b.sdql", "src/test/job/gj/results/28b.result")
//    Helpers.assertOutputs("progs/job/gj/28c.sdql", "src/test/job/gj/results/28c.result")
//
//    Helpers.assertOutputs("progs/job/gj/29a.sdql", "src/test/job/gj/results/29a.result")
//    Helpers.assertOutputs("progs/job/gj/29b.sdql", "src/test/job/gj/results/29b.result")
//    Helpers.assertOutputs("progs/job/gj/29c.sdql", "src/test/job/gj/results/29c.result")
//
//    Helpers.assertOutputs("progs/job/gj/30a.sdql", "src/test/job/gj/results/30a.result")
//    Helpers.assertOutputs("progs/job/gj/30b.sdql", "src/test/job/gj/results/30b.result")
//    Helpers.assertOutputs("progs/job/gj/30c.sdql", "src/test/job/gj/results/30c.result")
//
//    Helpers.assertOutputs("progs/job/gj/31a.sdql", "src/test/job/gj/results/31a.result")
//    Helpers.assertOutputs("progs/job/gj/31b.sdql", "src/test/job/gj/results/31b.result")
//    Helpers.assertOutputs("progs/job/gj/31c.sdql", "src/test/job/gj/results/31c.result")
//
//    Helpers.assertOutputs("progs/job/gj/32b.sdql", "src/test/job/gj/results/32b.result")
//
//    Helpers.assertOutputs("progs/job/gj/33a.sdql", "src/test/job/gj/results/33a.result")
//    Helpers.assertOutputs("progs/job/gj/33b.sdql", "src/test/job/gj/results/33b.result")
//    Helpers.assertOutputs("progs/job/gj/33c.sdql", "src/test/job/gj/results/33c.result")
//  }
//}

object Helpers {
  def assertOutputs(sdqlPath: String, outPath: String, sf: String = ""): Unit = {
    val source_code = SourceCode.fromFile(sdqlPath, if(sf.isEmpty) identity else patch(sf))
    val cpp = CppCodegen(source_code.exp)
    val actual = compile(sdqlPath, cpp)
    val source = scala.io.Source.fromFile(outPath)
    val expected = try source.mkString finally source.close()
    assert(actual.trim == expected.trim, s"\n${actual.trim}\n!=\n${expected.trim}")
  }
  private def patch(sf: String)(s:String) = s.replace("datasets/tpch/", s"src/test/tpch/data/SF_$sf/")
}
