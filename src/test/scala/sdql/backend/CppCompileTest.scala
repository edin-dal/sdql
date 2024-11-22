package sdql
package backend
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{ BeforeAndAfterAll, ParallelTestExecution, Tag }
import sdql.backend.CppCompile.compile
import sdql.frontend.SourceCode
import sdql.transformations.Rewriter

import scala.sys.process.Process

class CppCompileTestTPCH0_01 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect TPCH files with scaling factor 0.01 to be present in your datasets folder
  private object TestTPCH0_01 extends Tag("TestTPCH0_01")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.TPCH_SUBFOLDER)(sf = 0.01)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.TPCH_SUBFOLDER)()

  it should "compile and run TPCH Q1 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q1.sdql", "results/tpch/SF_0.01/q1.result")
  }
  it should "compile and run TPCH Q2 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q2.sdql", "results/tpch/SF_0.01/q2.result")
  }
  it should "compile and run TPCH Q3 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q3.sdql", "results/tpch/SF_0.01/q3.result")
  }
  it should "compile and run TPCH Q4 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q4.sdql", "results/tpch/SF_0.01/q4.result")
  }
  it should "compile and run TPCH Q5 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q5.sdql", "results/tpch/SF_0.01/q5.result")
  }
  it should "compile and run TPCH Q6 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q6.sdql", "results/tpch/SF_0.01/q6.result")
  }
  it should "compile and run TPCH Q7 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q7.sdql", "results/tpch/SF_0.01/q7.result")
  }
  it should "compile and run TPCH Q8 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q8.sdql", "results/tpch/SF_0.01/q8.result")
  }
  it should "compile and run TPCH Q9 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q9.sdql", "results/tpch/SF_0.01/q9.result")
  }
  it should "compile and run TPCH Q10 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q10.sdql", "results/tpch/SF_0.01/q10.result")
  }
  it should "compile and run TPCH Q11 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q11.sdql", "results/tpch/SF_0.01/q11.result")
  }
  it should "compile and run TPCH Q12 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q12.sdql", "results/tpch/SF_0.01/q12.result")
  }
  it should "compile and run TPCH Q13 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q13.sdql", "results/tpch/SF_0.01/q13.result")
  }
  it should "compile and run TPCH Q14 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q14.sdql", "results/tpch/SF_0.01/q14.result")
  }
  it should "compile and run TPCH Q15 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q15.sdql", "results/tpch/SF_0.01/q15.result")
  }
  it should "compile and run TPCH Q16 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q16.sdql", "results/tpch/SF_0.01/q16.result")
  }
  it should "compile and run TPCH Q17 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q17.sdql", "results/tpch/SF_0.01/q17.result")
  }
  it should "compile and run TPCH Q18 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q18.sdql", "results/tpch/SF_0.01/q18.result")
  }
  it should "compile and run TPCH Q19 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q19.sdql", "results/tpch/SF_0.01/q19.result")
  }
  it should "compile and run TPCH Q20 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q20.sdql", "results/tpch/SF_0.01/q20.result")
  }
  it should "compile and run TPCH Q21 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q21.sdql", "results/tpch/SF_0.01/q21.result")
  }
  it should "compile and run TPCH Q22 SF=0.01" taggedAs TestTPCH0_01 in {
    CompileHelpers.assertOutputs("progs/tpch/q22.sdql", "results/tpch/SF_0.01/q22.result")
  }
}

// note: DON'T run all cases in parallel - unless you have ~64GB of ram
class CppCompileTestTPCH1 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect TPCH files with scaling factor 1 to be present in your datasets folder
  private object TestTPCH1 extends Tag("TestTPCH1")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.TPCH_SUBFOLDER)(sf = 1)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.TPCH_SUBFOLDER)()

  it should "compile and run TPCH Q1-2 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q1.sdql", "results/tpch/SF_1/q1.result")
    CompileHelpers.assertOutputs("progs/tpch/q2.sdql", "results/tpch/SF_1/q2.result")
  }
  it should "compile and run TPCH Q3-4 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q3.sdql", "results/tpch/SF_1/q3.result")
    CompileHelpers.assertOutputs("progs/tpch/q4.sdql", "results/tpch/SF_1/q4.result")
  }
  it should "compile and run TPCH Q5-6 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q5.sdql", "results/tpch/SF_1/q5.result")
    CompileHelpers.assertOutputs("progs/tpch/q6.sdql", "results/tpch/SF_1/q6.result")
  }
  it should "compile and run TPCH Q7-8 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q7.sdql", "results/tpch/SF_1/q7.result")
    CompileHelpers.assertOutputs("progs/tpch/q8.sdql", "results/tpch/SF_1/q8.result")
  }
  it should "compile and run TPCH Q9-10 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q9.sdql", "results/tpch/SF_1/q9.result")
    CompileHelpers.assertOutputs("progs/tpch/q10.sdql", "results/tpch/SF_1/q10.result")
  }
  it should "compile and run TPCH Q11-12 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q11.sdql", "results/tpch/SF_1/q11.result")
    CompileHelpers.assertOutputs("progs/tpch/q12.sdql", "results/tpch/SF_1/q12.result")
  }
  it should "compile and run TPCH Q13-14 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q13.sdql", "results/tpch/SF_1/q13.result")
    CompileHelpers.assertOutputs("progs/tpch/q14.sdql", "results/tpch/SF_1/q14.result")
  }
  it should "compile and run TPCH Q15-16 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q15.sdql", "results/tpch/SF_1/q15.result")
    CompileHelpers.assertOutputs("progs/tpch/q16.sdql", "results/tpch/SF_1/q16.result")

  }
  it should "compile and run TPCH 17-18 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q17.sdql", "results/tpch/SF_1/q17.result")
    CompileHelpers.assertOutputs("progs/tpch/q18.sdql", "results/tpch/SF_1/q18.result")
  }
  it should "compile and run TPCH Q19-20 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q19.sdql", "results/tpch/SF_1/q19.result")
    CompileHelpers.assertOutputs("progs/tpch/q20.sdql", "results/tpch/SF_1/q20.result")
  }
  it should "compile and run TPCH Q21-22 SF=1" taggedAs TestTPCH1 in {
    CompileHelpers.assertOutputs("progs/tpch/q21.sdql", "results/tpch/SF_1/q21.result")
    CompileHelpers.assertOutputs("progs/tpch/q22.sdql", "results/tpch/SF_1/q22.result")
  }
}

// manually load balanced - batched together to roughly same times
// note: DON'T run all cases in parallel - unless you have ~64GB of ram
class CppCompileTestJOBGJ extends AnyFlatSpec with ParallelTestExecution {

  object TestJOBGJ extends Tag("TestJOBGJ")

  // note: these tests expect JOB files to be present in your datasets folder
  it should "compile and run JOB GJ 1-5 & 8-15" taggedAs TestJOBGJ in {
    // 1-5
    CompileHelpers.assertOutputs("progs/job/gj/1a.sdql", "results/job/1a.result")
    CompileHelpers.assertOutputs("progs/job/gj/1b.sdql", "results/job/1b.result")
    CompileHelpers.assertOutputs("progs/job/gj/1c.sdql", "results/job/1c.result")
    CompileHelpers.assertOutputs("progs/job/gj/1d.sdql", "results/job/1d.result")

    CompileHelpers.assertOutputs("progs/job/gj/2a.sdql", "results/job/2a.result")
    CompileHelpers.assertOutputs("progs/job/gj/2b.sdql", "results/job/2b.result")
    CompileHelpers.assertOutputs("progs/job/gj/2d.sdql", "results/job/2d.result")

    CompileHelpers.assertOutputs("progs/job/gj/3a.sdql", "results/job/3a.result")
    CompileHelpers.assertOutputs("progs/job/gj/3b.sdql", "results/job/3b.result")
    CompileHelpers.assertOutputs("progs/job/gj/3c.sdql", "results/job/3c.result")

    CompileHelpers.assertOutputs("progs/job/gj/4a.sdql", "results/job/4a.result")
    CompileHelpers.assertOutputs("progs/job/gj/4b.sdql", "results/job/4b.result")
    CompileHelpers.assertOutputs("progs/job/gj/4c.sdql", "results/job/4c.result")

    CompileHelpers.assertOutputs("progs/job/gj/5c.sdql", "results/job/5c.result")

    // 8-15
    CompileHelpers.assertOutputs("progs/job/gj/8a.sdql", "results/job/8a.result")
    CompileHelpers.assertOutputs("progs/job/gj/8b.sdql", "results/job/8b.result")
    CompileHelpers.assertOutputs("progs/job/gj/8c.sdql", "results/job/8c.result")
    CompileHelpers.assertOutputs("progs/job/gj/8d.sdql", "results/job/8d.result")

    CompileHelpers.assertOutputs("progs/job/gj/9a.sdql", "results/job/9a.result")
    CompileHelpers.assertOutputs("progs/job/gj/9b.sdql", "results/job/9b.result")
    CompileHelpers.assertOutputs("progs/job/gj/9c.sdql", "results/job/9c.result")
    CompileHelpers.assertOutputs("progs/job/gj/9d.sdql", "results/job/9d.result")

    CompileHelpers.assertOutputs("progs/job/gj/10a.sdql", "results/job/10a.result")
    CompileHelpers.assertOutputs("progs/job/gj/10c.sdql", "results/job/10c.result")

    CompileHelpers.assertOutputs("progs/job/gj/11a.sdql", "results/job/11a.result")
    CompileHelpers.assertOutputs("progs/job/gj/11b.sdql", "results/job/11b.result")
    CompileHelpers.assertOutputs("progs/job/gj/11c.sdql", "results/job/11c.result")
    CompileHelpers.assertOutputs("progs/job/gj/11d.sdql", "results/job/11d.result")

    CompileHelpers.assertOutputs("progs/job/gj/12a.sdql", "results/job/12a.result")
    CompileHelpers.assertOutputs("progs/job/gj/12b.sdql", "results/job/12b.result")
    CompileHelpers.assertOutputs("progs/job/gj/12c.sdql", "results/job/12c.result")

    CompileHelpers.assertOutputs("progs/job/gj/13a.sdql", "results/job/13a.result")
    CompileHelpers.assertOutputs("progs/job/gj/13b.sdql", "results/job/13b.result")
    CompileHelpers.assertOutputs("progs/job/gj/13c.sdql", "results/job/13c.result")
    // note: producing_company min here is '68 Productions instead "O" Films due to ordering of ' < "
    CompileHelpers.assertOutputs("progs/job/gj/13d.sdql", "results/job/13d.result")

    CompileHelpers.assertOutputs("progs/job/gj/14a.sdql", "results/job/14a.result")
    CompileHelpers.assertOutputs("progs/job/gj/14b.sdql", "results/job/14b.result")
    CompileHelpers.assertOutputs("progs/job/gj/14c.sdql", "results/job/14c.result")

    CompileHelpers.assertOutputs("progs/job/gj/15a.sdql", "results/job/15a.result")
    CompileHelpers.assertOutputs("progs/job/gj/15b.sdql", "results/job/15b.result")
    CompileHelpers.assertOutputs("progs/job/gj/15c.sdql", "results/job/15c.result")
    CompileHelpers.assertOutputs("progs/job/gj/15d.sdql", "results/job/15d.result")
  }

  it should "compile and run JOB GJ 6-7" taggedAs TestJOBGJ in {
    CompileHelpers.assertOutputs("progs/job/gj/6a.sdql", "results/job/6a.result")
    CompileHelpers.assertOutputs("progs/job/gj/6b.sdql", "results/job/6b.result")
    CompileHelpers.assertOutputs("progs/job/gj/6c.sdql", "results/job/6c.result")
    CompileHelpers.assertOutputs("progs/job/gj/6d.sdql", "results/job/6d.result")
    CompileHelpers.assertOutputs("progs/job/gj/6e.sdql", "results/job/6e.result")
    CompileHelpers.assertOutputs("progs/job/gj/6f.sdql", "results/job/6f.result")

    CompileHelpers.assertOutputs("progs/job/gj/7a.sdql", "results/job/7a.result")
    CompileHelpers.assertOutputs("progs/job/gj/7b.sdql", "results/job/7b.result")
    CompileHelpers.assertOutputs("progs/job/gj/7c.sdql", "results/job/7c.result")
  }

  it should "compile and run JOB GJ 16-17" taggedAs TestJOBGJ in {
    CompileHelpers.assertOutputs("progs/job/gj/16a.sdql", "results/job/16a.result")
    CompileHelpers.assertOutputs("progs/job/gj/16b.sdql", "results/job/16b.result")
    CompileHelpers.assertOutputs("progs/job/gj/16c.sdql", "results/job/16c.result")
    CompileHelpers.assertOutputs("progs/job/gj/16d.sdql", "results/job/16d.result")

    CompileHelpers.assertOutputs("progs/job/gj/17a.sdql", "results/job/17a.result")
    CompileHelpers.assertOutputs("progs/job/gj/17b.sdql", "results/job/17b.result")
    CompileHelpers.assertOutputs("progs/job/gj/17c.sdql", "results/job/17c.result")
    CompileHelpers.assertOutputs("progs/job/gj/17d.sdql", "results/job/17d.result")
    CompileHelpers.assertOutputs("progs/job/gj/17e.sdql", "results/job/17e.result")
    CompileHelpers.assertOutputs("progs/job/gj/17f.sdql", "results/job/17f.result")
  }

  it should "compile and run JOB GJ 18-20 & 26" taggedAs TestJOBGJ in {
    // 18-20
    CompileHelpers.assertOutputs("progs/job/gj/18a.sdql", "results/job/18a.result")
    CompileHelpers.assertOutputs("progs/job/gj/18b.sdql", "results/job/18b.result")
    CompileHelpers.assertOutputs("progs/job/gj/18c.sdql", "results/job/18c.result")

    CompileHelpers.assertOutputs("progs/job/gj/19a.sdql", "results/job/19a.result")
    CompileHelpers.assertOutputs("progs/job/gj/19b.sdql", "results/job/19b.result")
    CompileHelpers.assertOutputs("progs/job/gj/19c.sdql", "results/job/19c.result")
    CompileHelpers.assertOutputs("progs/job/gj/19d.sdql", "results/job/19d.result")

    CompileHelpers.assertOutputs("progs/job/gj/20a.sdql", "results/job/20a.result")
    CompileHelpers.assertOutputs("progs/job/gj/20b.sdql", "results/job/20b.result")
    CompileHelpers.assertOutputs("progs/job/gj/20c.sdql", "results/job/20c.result")

    // 26
    CompileHelpers.assertOutputs("progs/job/gj/26a.sdql", "results/job/26a.result")
    CompileHelpers.assertOutputs("progs/job/gj/26b.sdql", "results/job/26b.result")
    CompileHelpers.assertOutputs("progs/job/gj/26c.sdql", "results/job/26c.result")
  }

  it should "compile and run JOB GJ 21-25 & 27-33" taggedAs TestJOBGJ in {
    // 21-25
    CompileHelpers.assertOutputs("progs/job/gj/21a.sdql", "results/job/21a.result")
    CompileHelpers.assertOutputs("progs/job/gj/21b.sdql", "results/job/21b.result")
    CompileHelpers.assertOutputs("progs/job/gj/21c.sdql", "results/job/21c.result")

    CompileHelpers.assertOutputs("progs/job/gj/22a.sdql", "results/job/22a.result")
    CompileHelpers.assertOutputs("progs/job/gj/22b.sdql", "results/job/22b.result")
    CompileHelpers.assertOutputs("progs/job/gj/22c.sdql", "results/job/22c.result")
    CompileHelpers.assertOutputs("progs/job/gj/22d.sdql", "results/job/22d.result")

    CompileHelpers.assertOutputs("progs/job/gj/23a.sdql", "results/job/23a.result")
    CompileHelpers.assertOutputs("progs/job/gj/23b.sdql", "results/job/23b.result")
    CompileHelpers.assertOutputs("progs/job/gj/23c.sdql", "results/job/23c.result")

    CompileHelpers.assertOutputs("progs/job/gj/24a.sdql", "results/job/24a.result")
    CompileHelpers.assertOutputs("progs/job/gj/24b.sdql", "results/job/24b.result")

    CompileHelpers.assertOutputs("progs/job/gj/25a.sdql", "results/job/25a.result")
    CompileHelpers.assertOutputs("progs/job/gj/25b.sdql", "results/job/25b.result")
    CompileHelpers.assertOutputs("progs/job/gj/25c.sdql", "results/job/25c.result")

    // 27-33
    CompileHelpers.assertOutputs("progs/job/gj/27a.sdql", "results/job/27a.result")
    CompileHelpers.assertOutputs("progs/job/gj/27b.sdql", "results/job/27b.result")
    CompileHelpers.assertOutputs("progs/job/gj/27c.sdql", "results/job/27c.result")

    CompileHelpers.assertOutputs("progs/job/gj/28a.sdql", "results/job/28a.result")
    CompileHelpers.assertOutputs("progs/job/gj/28b.sdql", "results/job/28b.result")
    CompileHelpers.assertOutputs("progs/job/gj/28c.sdql", "results/job/28c.result")

    CompileHelpers.assertOutputs("progs/job/gj/29a.sdql", "results/job/29a.result")
    CompileHelpers.assertOutputs("progs/job/gj/29b.sdql", "results/job/29b.result")
    CompileHelpers.assertOutputs("progs/job/gj/29c.sdql", "results/job/29c.result")

    CompileHelpers.assertOutputs("progs/job/gj/30a.sdql", "results/job/30a.result")
    CompileHelpers.assertOutputs("progs/job/gj/30b.sdql", "results/job/30b.result")
    CompileHelpers.assertOutputs("progs/job/gj/30c.sdql", "results/job/30c.result")

    CompileHelpers.assertOutputs("progs/job/gj/31a.sdql", "results/job/31a.result")
    CompileHelpers.assertOutputs("progs/job/gj/31b.sdql", "results/job/31b.result")
    CompileHelpers.assertOutputs("progs/job/gj/31c.sdql", "results/job/31c.result")

    CompileHelpers.assertOutputs("progs/job/gj/32b.sdql", "results/job/32b.result")

    CompileHelpers.assertOutputs("progs/job/gj/33a.sdql", "results/job/33a.result")
    CompileHelpers.assertOutputs("progs/job/gj/33b.sdql", "results/job/33b.result")
    CompileHelpers.assertOutputs("progs/job/gj/33c.sdql", "results/job/33c.result")
  }
}

// manually load balanced - based on the GJ queries (so not perfect here)
// note: DON'T run all cases in parallel - unless you have ~64GB of ram
class CppCompileTestJOBFJ extends AnyFlatSpec with ParallelTestExecution {

  // note: these tests expect JOB files to be present in your datasets folder
  private object TestJOBFJ extends Tag("TestJOBFJ")

  it should "compile and run JOB FJ 1-5 & 8-15" taggedAs TestJOBFJ in {
    // 1-5
    CompileHelpers.assertOutputs("progs/job/fj/1a.sdql", "results/job/1a.result")
    CompileHelpers.assertOutputs("progs/job/fj/1b.sdql", "results/job/1b.result")
    CompileHelpers.assertOutputs("progs/job/fj/1c.sdql", "results/job/1c.result")
    CompileHelpers.assertOutputs("progs/job/fj/1d.sdql", "results/job/1d.result")

    CompileHelpers.assertOutputs("progs/job/fj/2a.sdql", "results/job/2a.result")
    CompileHelpers.assertOutputs("progs/job/fj/2b.sdql", "results/job/2b.result")
    CompileHelpers.assertOutputs("progs/job/fj/2d.sdql", "results/job/2d.result")

    CompileHelpers.assertOutputs("progs/job/fj/3a.sdql", "results/job/3a.result")
    CompileHelpers.assertOutputs("progs/job/fj/3b.sdql", "results/job/3b.result")
    CompileHelpers.assertOutputs("progs/job/fj/3c.sdql", "results/job/3c.result")

    CompileHelpers.assertOutputs("progs/job/fj/4a.sdql", "results/job/4a.result")
    CompileHelpers.assertOutputs("progs/job/fj/4b.sdql", "results/job/4b.result")
    CompileHelpers.assertOutputs("progs/job/fj/4c.sdql", "results/job/4c.result")

    CompileHelpers.assertOutputs("progs/job/fj/5c.sdql", "results/job/5c.result")

    // 8-15
    CompileHelpers.assertOutputs("progs/job/fj/8a.sdql", "results/job/8a.result")
    CompileHelpers.assertOutputs("progs/job/fj/8b.sdql", "results/job/8b.result")
    CompileHelpers.assertOutputs("progs/job/fj/8c.sdql", "results/job/8c.result")
    CompileHelpers.assertOutputs("progs/job/fj/8d.sdql", "results/job/8d.result")

    CompileHelpers.assertOutputs("progs/job/fj/9a.sdql", "results/job/9a.result")
    CompileHelpers.assertOutputs("progs/job/fj/9b.sdql", "results/job/9b.result")
    CompileHelpers.assertOutputs("progs/job/fj/9c.sdql", "results/job/9c.result")
    CompileHelpers.assertOutputs("progs/job/fj/9d.sdql", "results/job/9d.result")

    CompileHelpers.assertOutputs("progs/job/fj/10a.sdql", "results/job/10a.result")
    CompileHelpers.assertOutputs("progs/job/fj/10c.sdql", "results/job/10c.result")

    CompileHelpers.assertOutputs("progs/job/fj/11a.sdql", "results/job/11a.result")
    CompileHelpers.assertOutputs("progs/job/fj/11b.sdql", "results/job/11b.result")
    CompileHelpers.assertOutputs("progs/job/fj/11c.sdql", "results/job/11c.result")
    CompileHelpers.assertOutputs("progs/job/fj/11d.sdql", "results/job/11d.result")

    CompileHelpers.assertOutputs("progs/job/fj/12a.sdql", "results/job/12a.result")
    CompileHelpers.assertOutputs("progs/job/fj/12b.sdql", "results/job/12b.result")
    CompileHelpers.assertOutputs("progs/job/fj/12c.sdql", "results/job/12c.result")

    CompileHelpers.assertOutputs("progs/job/fj/13a.sdql", "results/job/13a.result")
    CompileHelpers.assertOutputs("progs/job/fj/13b.sdql", "results/job/13b.result")
    CompileHelpers.assertOutputs("progs/job/fj/13c.sdql", "results/job/13c.result")
    // note: producing_company min here is '68 Productions instead "O" Films due to ordering of ' < "
    CompileHelpers.assertOutputs("progs/job/fj/13d.sdql", "results/job/13d.result")

    CompileHelpers.assertOutputs("progs/job/fj/14a.sdql", "results/job/14a.result")
    CompileHelpers.assertOutputs("progs/job/fj/14b.sdql", "results/job/14b.result")
    CompileHelpers.assertOutputs("progs/job/fj/14c.sdql", "results/job/14c.result")

    CompileHelpers.assertOutputs("progs/job/fj/15a.sdql", "results/job/15a.result")
    CompileHelpers.assertOutputs("progs/job/fj/15b.sdql", "results/job/15b.result")
    CompileHelpers.assertOutputs("progs/job/fj/15c.sdql", "results/job/15c.result")
    CompileHelpers.assertOutputs("progs/job/fj/15d.sdql", "results/job/15d.result")
  }

  it should "compile and run JOB FJ 6-7" taggedAs TestJOBFJ in {
    CompileHelpers.assertOutputs("progs/job/fj/6a.sdql", "results/job/6a.result")
    CompileHelpers.assertOutputs("progs/job/fj/6b.sdql", "results/job/6b.result")
    CompileHelpers.assertOutputs("progs/job/fj/6c.sdql", "results/job/6c.result")
    CompileHelpers.assertOutputs("progs/job/fj/6d.sdql", "results/job/6d.result")
    CompileHelpers.assertOutputs("progs/job/fj/6e.sdql", "results/job/6e.result")
    CompileHelpers.assertOutputs("progs/job/fj/6f.sdql", "results/job/6f.result")

    CompileHelpers.assertOutputs("progs/job/fj/7a.sdql", "results/job/7a.result")
    CompileHelpers.assertOutputs("progs/job/fj/7b.sdql", "results/job/7b.result")
    CompileHelpers.assertOutputs("progs/job/fj/7c.sdql", "results/job/7c.result")
  }

  it should "compile and run JOB FJ 16-17" taggedAs TestJOBFJ in {
    CompileHelpers.assertOutputs("progs/job/fj/16a.sdql", "results/job/16a.result")
    CompileHelpers.assertOutputs("progs/job/fj/16b.sdql", "results/job/16b.result")
    CompileHelpers.assertOutputs("progs/job/fj/16c.sdql", "results/job/16c.result")
    CompileHelpers.assertOutputs("progs/job/fj/16d.sdql", "results/job/16d.result")

    CompileHelpers.assertOutputs("progs/job/fj/17a.sdql", "results/job/17a.result")
    CompileHelpers.assertOutputs("progs/job/fj/17b.sdql", "results/job/17b.result")
    CompileHelpers.assertOutputs("progs/job/fj/17c.sdql", "results/job/17c.result")
    CompileHelpers.assertOutputs("progs/job/fj/17d.sdql", "results/job/17d.result")
    CompileHelpers.assertOutputs("progs/job/fj/17e.sdql", "results/job/17e.result")
    CompileHelpers.assertOutputs("progs/job/fj/17f.sdql", "results/job/17f.result")
  }

  it should "compile and run JOB FJ 18-20 & 26" taggedAs TestJOBFJ in {
    // 18-20
    CompileHelpers.assertOutputs("progs/job/fj/18a.sdql", "results/job/18a.result")
    CompileHelpers.assertOutputs("progs/job/fj/18b.sdql", "results/job/18b.result")
    CompileHelpers.assertOutputs("progs/job/fj/18c.sdql", "results/job/18c.result")

    CompileHelpers.assertOutputs("progs/job/fj/19a.sdql", "results/job/19a.result")
    CompileHelpers.assertOutputs("progs/job/fj/19b.sdql", "results/job/19b.result")
    CompileHelpers.assertOutputs("progs/job/fj/19c.sdql", "results/job/19c.result")
    CompileHelpers.assertOutputs("progs/job/fj/19d.sdql", "results/job/19d.result")

    CompileHelpers.assertOutputs("progs/job/fj/20a.sdql", "results/job/20a.result")
    CompileHelpers.assertOutputs("progs/job/fj/20b.sdql", "results/job/20b.result")
    CompileHelpers.assertOutputs("progs/job/fj/20c.sdql", "results/job/20c.result")

    // 26
    CompileHelpers.assertOutputs("progs/job/fj/26a.sdql", "results/job/26a.result")
    CompileHelpers.assertOutputs("progs/job/fj/26b.sdql", "results/job/26b.result")
    CompileHelpers.assertOutputs("progs/job/fj/26c.sdql", "results/job/26c.result")
  }

  it should "compile and run JOB FJ 21-25 & 27-33" taggedAs TestJOBFJ in {
    // 21-25
    CompileHelpers.assertOutputs("progs/job/fj/21a.sdql", "results/job/21a.result")
    CompileHelpers.assertOutputs("progs/job/fj/21b.sdql", "results/job/21b.result")
    CompileHelpers.assertOutputs("progs/job/fj/21c.sdql", "results/job/21c.result")

    CompileHelpers.assertOutputs("progs/job/fj/22a.sdql", "results/job/22a.result")
    CompileHelpers.assertOutputs("progs/job/fj/22b.sdql", "results/job/22b.result")
    CompileHelpers.assertOutputs("progs/job/fj/22c.sdql", "results/job/22c.result")
    CompileHelpers.assertOutputs("progs/job/fj/22d.sdql", "results/job/22d.result")

    CompileHelpers.assertOutputs("progs/job/fj/23a.sdql", "results/job/23a.result")
    CompileHelpers.assertOutputs("progs/job/fj/23b.sdql", "results/job/23b.result")
    CompileHelpers.assertOutputs("progs/job/fj/23c.sdql", "results/job/23c.result")

    CompileHelpers.assertOutputs("progs/job/fj/24a.sdql", "results/job/24a.result")
    CompileHelpers.assertOutputs("progs/job/fj/24b.sdql", "results/job/24b.result")

    CompileHelpers.assertOutputs("progs/job/fj/25a.sdql", "results/job/25a.result")
    CompileHelpers.assertOutputs("progs/job/fj/25b.sdql", "results/job/25b.result")
    CompileHelpers.assertOutputs("progs/job/fj/25c.sdql", "results/job/25c.result")

    // 27-33
    CompileHelpers.assertOutputs("progs/job/fj/27a.sdql", "results/job/27a.result")
    CompileHelpers.assertOutputs("progs/job/fj/27b.sdql", "results/job/27b.result")
    CompileHelpers.assertOutputs("progs/job/fj/27c.sdql", "results/job/27c.result")

    CompileHelpers.assertOutputs("progs/job/fj/28a.sdql", "results/job/28a.result")
    CompileHelpers.assertOutputs("progs/job/fj/28b.sdql", "results/job/28b.result")
    CompileHelpers.assertOutputs("progs/job/fj/28c.sdql", "results/job/28c.result")

    CompileHelpers.assertOutputs("progs/job/fj/29a.sdql", "results/job/29a.result")
    CompileHelpers.assertOutputs("progs/job/fj/29b.sdql", "results/job/29b.result")
    CompileHelpers.assertOutputs("progs/job/fj/29c.sdql", "results/job/29c.result")

    CompileHelpers.assertOutputs("progs/job/fj/30a.sdql", "results/job/30a.result")
    CompileHelpers.assertOutputs("progs/job/fj/30b.sdql", "results/job/30b.result")
    CompileHelpers.assertOutputs("progs/job/fj/30c.sdql", "results/job/30c.result")

    CompileHelpers.assertOutputs("progs/job/fj/31a.sdql", "results/job/31a.result")
    CompileHelpers.assertOutputs("progs/job/fj/31b.sdql", "results/job/31b.result")
    CompileHelpers.assertOutputs("progs/job/fj/31c.sdql", "results/job/31c.result")

    CompileHelpers.assertOutputs("progs/job/fj/32b.sdql", "results/job/32b.result")

    CompileHelpers.assertOutputs("progs/job/fj/33a.sdql", "results/job/33a.result")
    CompileHelpers.assertOutputs("progs/job/fj/33b.sdql", "results/job/33b.result")
    CompileHelpers.assertOutputs("progs/job/fj/33c.sdql", "results/job/33c.result")
  }
}

class CppCompileTestLSQBGJ0_1 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 0.1 to be present in your datasets folder
  private object TestLSQBGJ0_1 extends Tag("TestLSQBGJ0_1")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 0.1)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB GJ Q1 SF=0.1" taggedAs TestLSQBGJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q1.sdql", "results/lsqb/SF_0.1/q1.result")
  }
  it should "compile and run LSQB GJ Q2 SF=0.1" taggedAs TestLSQBGJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q2.sdql", "results/lsqb/SF_0.1/q2.result")
  }
  it should "compile and run LSQB GJ Q4 SF=0.1" taggedAs TestLSQBGJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q4.sdql", "results/lsqb/SF_0.1/q4.result")
  }
  it should "compile and run LSQB GJ Q5 SF=0.1" taggedAs TestLSQBGJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q5.sdql", "results/lsqb/SF_0.1/q5.result")
  }
}
class CppCompileTestLSQBFJ0_1 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 0.1 to be present in your datasets folder
  private object TestLSQBFJ0_1 extends Tag("TestLSQBFJ0_1")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 0.1)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB FJ Q1 SF=0.1" taggedAs TestLSQBFJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q1.sdql", "results/lsqb/SF_0.1/q1.result")
  }
  it should "compile and run LSQB FJ Q2 SF=0.1" taggedAs TestLSQBFJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q2.sdql", "results/lsqb/SF_0.1/q2.result")
  }
  it should "compile and run LSQB FJ Q4 SF=0.1" taggedAs TestLSQBFJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q4.sdql", "results/lsqb/SF_0.1/q4.result")
  }
  it should "compile and run LSQB FJ Q5 SF=0.1" taggedAs TestLSQBFJ0_1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q5.sdql", "results/lsqb/SF_0.1/q5.result")
  }
}

class CppCompileTestLSQBGJ0_3 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 0.3 to be present in your datasets folder
  private object TestLSQBGJ0_3 extends Tag("TestLSQBGJ0_3")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 0.3)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB GJ Q1 SF=0.3" taggedAs TestLSQBGJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q1.sdql", "results/lsqb/SF_0.3/q1.result")
  }
  it should "compile and run LSQB GJ Q2 SF=0.3" taggedAs TestLSQBGJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q2.sdql", "results/lsqb/SF_0.3/q2.result")
  }
  it should "compile and run LSQB GJ Q4 SF=0.3" taggedAs TestLSQBGJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q4.sdql", "results/lsqb/SF_0.3/q4.result")
  }
  it should "compile and run LSQB GJ Q5 SF=0.3" taggedAs TestLSQBGJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q5.sdql", "results/lsqb/SF_0.3/q5.result")
  }
}
class CppCompileTestLSQBFJ0_3 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 0.3 to be present in your datasets folder
  private object TestLSQBFJ0_3 extends Tag("TestLSQBFJ0_3")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 0.3)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB FJ Q1 SF=0.3" taggedAs TestLSQBFJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q1.sdql", "results/lsqb/SF_0.3/q1.result")
  }
  it should "compile and run LSQB FJ Q2 SF=0.3" taggedAs TestLSQBFJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q2.sdql", "results/lsqb/SF_0.3/q2.result")
  }
  it should "compile and run LSQB FJ Q4 SF=0.3" taggedAs TestLSQBFJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q4.sdql", "results/lsqb/SF_0.3/q4.result")
  }
  it should "compile and run LSQB FJ Q5 SF=0.3" taggedAs TestLSQBFJ0_3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q5.sdql", "results/lsqb/SF_0.3/q5.result")
  }
}

class CppCompileTestLSQBGJ1 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 1 to be present in your datasets folder
  private object TestLSQBGJ1 extends Tag("TestLSQBGJ1")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 1)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB GJ Q1 SF=1" taggedAs TestLSQBGJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q1.sdql", "results/lsqb/SF_1/q1.result")
  }
  it should "compile and run LSQB GJ Q2 SF=1" taggedAs TestLSQBGJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q2.sdql", "results/lsqb/SF_1/q2.result")
  }
  it should "compile and run LSQB GJ Q4 SF=1" taggedAs TestLSQBGJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q4.sdql", "results/lsqb/SF_1/q4.result")
  }
  it should "compile and run LSQB GJ Q5 SF=1" taggedAs TestLSQBGJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q5.sdql", "results/lsqb/SF_1/q5.result")
  }
}
class CppCompileTestLSQBFJ1 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 1 to be present in your datasets folder
  private object TestLSQBFJ1 extends Tag("TestLSQBFJ1")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 1)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB FJ Q1 SF=1" taggedAs TestLSQBFJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q1.sdql", "results/lsqb/SF_1/q1.result")
  }
  it should "compile and run LSQB FJ Q2 SF=1" taggedAs TestLSQBFJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q2.sdql", "results/lsqb/SF_1/q2.result")
  }
  it should "compile and run LSQB FJ Q4 SF=1" taggedAs TestLSQBFJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q4.sdql", "results/lsqb/SF_1/q4.result")
  }
  it should "compile and run LSQB FJ Q5 SF=1" taggedAs TestLSQBFJ1 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q5.sdql", "results/lsqb/SF_1/q5.result")
  }
}

class CppCompileTestLSQBGJ3 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 3 to be present in your datasets folder
  private object TestLSQBGJ3 extends Tag("TestLSQBGJ3")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 3)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB GJ Q1 SF=3" taggedAs TestLSQBGJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q1.sdql", "results/lsqb/SF_3/q1.result")
  }
  it should "compile and run LSQB GJ Q2 SF=3" taggedAs TestLSQBGJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q2.sdql", "results/lsqb/SF_3/q2.result")
  }
  it should "compile and run LSQB GJ Q4 SF=3" taggedAs TestLSQBGJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q4.sdql", "results/lsqb/SF_3/q4.result")
  }
  it should "compile and run LSQB GJ Q5 SF=3" taggedAs TestLSQBGJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/gj/q5.sdql", "results/lsqb/SF_3/q5.result")
  }
}
class CppCompileTestLSQBFJ3 extends AnyFlatSpec with ParallelTestExecution with BeforeAndAfterAll {

  // note: these tests expect LSQB files with scaling factor 3 to be present in your datasets folder
  private object TestLSQBFJ3 extends Tag("TestLSQBFJ3")

  override def beforeAll(): Unit = DatasetsHelpers.beforeAll(DatasetsHelpers.LSQB_SUBFOLDER)(sf = 3)
  override def afterAll(): Unit  = DatasetsHelpers.afterAll(DatasetsHelpers.LSQB_SUBFOLDER)()

  it should "compile and run LSQB FJ Q1 SF=3" taggedAs TestLSQBFJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q1.sdql", "results/lsqb/SF_3/q1.result")
  }
  it should "compile and run LSQB FJ Q2 SF=3" taggedAs TestLSQBFJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q2.sdql", "results/lsqb/SF_3/q2.result")
  }
  it should "compile and run LSQB FJ Q4 SF=3" taggedAs TestLSQBFJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q4.sdql", "results/lsqb/SF_3/q4.result")
  }
  it should "compile and run LSQB FJ Q5 SF=3" taggedAs TestLSQBFJ3 in {
    CompileHelpers.assertOutputs("progs/lsqb/fj/q5.sdql", "results/lsqb/SF_3/q5.result")
  }
}

class CppCompileTestJOBSorting extends AnyFlatSpec with ParallelTestExecution {

  object TestJOBSorting extends Tag("TestJOBSorting")

  it should "compile and run JOB 3a" taggedAs TestJOBSorting in {
    CompileHelpers.assertOutputs("progs/sorting/job/fj_pure/3a.sdql", "results/job/3a.result")
    CompileHelpers.assertOutputs("progs/sorting/job/fj_hybrid/3a.sdql", "results/job/3a.result")
    CompileHelpers.assertOutputs("progs/sorting/job/gj_hybrid/3a.sdql", "results/job/3a.result")
    CompileHelpers.assertOutputs("progs/sorting/job/gj_hybrid/3a_optimised.sdql", "results/job/3a.result")
  }

  it should "compile and run JOB 8a" taggedAs TestJOBSorting in {
    CompileHelpers.assertOutputs("progs/sorting/job/fj_pure/8a.sdql", "results/job/8a.result")
    CompileHelpers.assertOutputs("progs/sorting/job/fj_hybrid/8a.sdql", "results/job/8a.result")
  }

  it should "compile and run JOB FJ 10a pure sorting" taggedAs TestJOBSorting in CompileHelpers.assertOutputs(
    "progs/sorting/job/fj_pure/10a.sdql",
    "results/job/10a.result"
  )

  it should "compile and run JOB 12b" taggedAs TestJOBSorting in {
    CompileHelpers.assertOutputs("progs/sorting/job/fj_pure/12b.sdql", "results/job/12b.result")
    CompileHelpers.assertOutputs("progs/sorting/job/fj_hybrid/12b.sdql", "results/job/12b.result")
  }

  it should "compile and run JOB GJ 13a hybrid sorting" taggedAs TestJOBSorting in {
    CompileHelpers.assertOutputs("progs/sorting/job/gj_hybrid/13a.sdql", "results/job/13a.result")
    CompileHelpers.assertOutputs("progs/sorting/job/gj_hybrid/13a_optimised.sdql", "results/job/13a.result")
  }

  it should "compile and run JOB GJ 13b hybrid sorting" taggedAs TestJOBSorting in CompileHelpers.assertOutputs(
    "progs/sorting/job/gj_hybrid/13b.sdql",
    "results/job/13b.result"
  )

  it should "compile and run JOB FJ 17b pure sorting" taggedAs TestJOBSorting in CompileHelpers.assertOutputs(
    "progs/sorting/job/fj_pure/17b.sdql",
    "results/job/17b.result"
  )

  it should "compile and run JOB FJ 17f pure sorting" taggedAs TestJOBSorting in CompileHelpers.assertOutputs(
    "progs/sorting/job/fj_pure/17f.sdql",
    "results/job/17f.result"
  )
}

object DatasetsHelpers {

  sealed trait Dataset
  case object JOB  extends Dataset
  case object LSQB extends Dataset

  // Convenience for running many tests, sets up / tears down a symlink to datasets with the appropriate scaling factors
  val TPCH_SUBFOLDER: Option[String] = None // e.g. set to Some("tpch") to read from datasets/tpch_datasets/
  val LSQB_SUBFOLDER: Option[String] = None // e.g. set to Some("lsqb") to read from datasets/lsqb_datasets/

  def beforeAll(subFolder: Option[String])(sf: Double): Unit = subFolder match {
    case Some(jobOrLsqb) =>
      val path = s"${jobOrLsqb}_datasets/SF_${toString(sf)}"
      val _    = Process(Seq("ln", "-s", path, jobOrLsqb), new java.io.File("datasets")).!!
    case _               =>
  }
  def afterAll(subFolder: Option[String])(): Unit            = subFolder match {
    case Some(jobOrLsqb) => val _ = Process(Seq("rm", jobOrLsqb), new java.io.File("datasets")).!!
    case _               =>
  }

  private def toString(sf: Double) = if (sf == sf.toInt) sf.toInt.toString else sf.toString
}

object CompileHelpers {
  def assertOutputs(sdqlPath: String, outPath: String): Unit = {
    val code     = SourceCode.fromFile(sdqlPath)
    val llql     = Rewriter.rewrite(code.exp)
    val cpp      = CppCodegen(llql)
    val actual   = compile(sdqlPath, cpp)
    val source   = scala.io.Source.fromFile(outPath)
    val expected =
      try source.mkString
      finally source.close()
    assert(actual.trim == expected.trim, s"\n${actual.trim}\n!=\n${expected.trim}")
  }
}
