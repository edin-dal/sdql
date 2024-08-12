package sdql
package backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{ ParallelTestExecution, Tag }
import sdql.backend.CppCompile.compile
import sdql.frontend.SourceCode

class CppCompileTestTPCH0_01 extends AnyFlatSpec with ParallelTestExecution {

  it should "compile and run TPCH Q1 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/results/SF_0.01/q1.result", sf = "0.01")
  }

  it should "compile and run TPCH Q2 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/results/SF_0.01/q2.result", sf = "0.01")
  }

  it should "compile and run TPCH Q3 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/results/SF_0.01/q3.result", sf = "0.01")
  }

  it should "compile and run TPCH Q4 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/results/SF_0.01/q4.result", sf = "0.01")
  }

  it should "compile and run TPCH Q5 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/results/SF_0.01/q5.result", sf = "0.01")
  }

  it should "compile and run TPCH Q6 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/results/SF_0.01/q6.result", sf = "0.01")
  }

  it should "compile and run TPCH Q7 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/results/SF_0.01/q7.result", sf = "0.01")
  }

  it should "compile and run TPCH Q8 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/results/SF_0.01/q8.result", sf = "0.01")
  }

  it should "compile and run TPCH Q9 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/results/SF_0.01/q9.result", sf = "0.01")
  }

  it should "compile and run TPCH Q10 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/results/SF_0.01/q10.result", sf = "0.01")
  }

  it should "compile and run TPCH Q11 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/results/SF_0.01/q11.result", sf = "0.01")
  }

  it should "compile and run TPCH Q12 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/results/SF_0.01/q12.result", sf = "0.01")
  }

  it should "compile and run TPCH Q13 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/results/SF_0.01/q13.result", sf = "0.01")
  }

  it should "compile and run TPCH Q14 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/results/SF_0.01/q14.result", sf = "0.01")
  }

  it should "compile and run TPCH Q15 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/results/SF_0.01/q15.result", sf = "0.01")
  }

  it should "compile and run TPCH Q16 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/results/SF_0.01/q16.result", sf = "0.01")
  }

  it should "compile and run TPCH Q17 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/results/SF_0.01/q17.result", sf = "0.01")
  }

  it should "compile and run TPCH Q18 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/results/SF_0.01/q18.result", sf = "0.01")
  }

  it should "compile and run TPCH Q19 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/results/SF_0.01/q19.result", sf = "0.01")
  }

  it should "compile and run TPCH Q20 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/results/SF_0.01/q20.result", sf = "0.01")
  }

  it should "compile and run TPCH Q21 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/results/SF_0.01/q21.result", sf = "0.01")
  }

  it should "compile and run TPCH Q22 SF=0.01" in {
    CompileHelpers.assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/results/SF_0.01/q22.result", sf = "0.01")
  }
}

// run these tests on dbgen TPCH datasets with scaling factor 1 in src/test/tpch/data/SF_1/
// also run this test separately from the SF=0.01 ones above - there's a concurrency bug when patching paths
// note: DON'T run them all in parallel - unless you have ~64GB of ram
class CppCompileTestTPCH1 extends AnyFlatSpec with ParallelTestExecution {

  object SlowTPCH extends Tag("SlowTPCH")

  it should "compile and run TPCH Q1-2 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q1.sdql", "src/test/tpch/results/SF_1/q1.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q2.sdql", "src/test/tpch/results/SF_1/q2.result", sf = "1")
  }

  it should "compile and run TPCH Q3-4 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q3.sdql", "src/test/tpch/results/SF_1/q3.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q4.sdql", "src/test/tpch/results/SF_1/q4.result", sf = "1")
  }

  it should "compile and run TPCH Q5-6 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q5.sdql", "src/test/tpch/results/SF_1/q5.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q6.sdql", "src/test/tpch/results/SF_1/q6.result", sf = "1")
  }

  it should "compile and run TPCH Q7-8 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q7.sdql", "src/test/tpch/results/SF_1/q7.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q8.sdql", "src/test/tpch/results/SF_1/q8.result", sf = "1")
  }

  it should "compile and run TPCH Q9-10 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q9.sdql", "src/test/tpch/results/SF_1/q9.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q10.sdql", "src/test/tpch/results/SF_1/q10.result", sf = "1")
  }

  it should "compile and run TPCH Q11-12 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q11.sdql", "src/test/tpch/results/SF_1/q11.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q12.sdql", "src/test/tpch/results/SF_1/q12.result", sf = "1")
  }

  it should "compile and run TPCH Q13-14 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q13.sdql", "src/test/tpch/results/SF_1/q13.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q14.sdql", "src/test/tpch/results/SF_1/q14.result", sf = "1")
  }

  it should "compile and run TPCH Q15-16 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q15.sdql", "src/test/tpch/results/SF_1/q15.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q16.sdql", "src/test/tpch/results/SF_1/q16.result", sf = "1")

  }

  it should "compile and run TPCH 17-18 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q17.sdql", "src/test/tpch/results/SF_1/q17.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q18.sdql", "src/test/tpch/results/SF_1/q18.result", sf = "1")
  }

  it should "compile and run TPCH Q19-20 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q19.sdql", "src/test/tpch/results/SF_1/q19.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q20.sdql", "src/test/tpch/results/SF_1/q20.result", sf = "1")
  }

  it should "compile and run TPCH Q21-22 SF=1" taggedAs SlowTPCH in {
    CompileHelpers.assertOutputs("progs/tpch/q21.sdql", "src/test/tpch/results/SF_1/q21.result", sf = "1")
    CompileHelpers.assertOutputs("progs/tpch/q22.sdql", "src/test/tpch/results/SF_1/q22.result", sf = "1")
  }
}

// run these tests on pre-processed datasets in datasets/job
// manually load balanced - batched together to roughly same times
// DON'T run them in parallel - unless you have ~64GB of ram
class CppCompileTestGJ extends AnyFlatSpec with ParallelTestExecution {

  object SlowGJ extends Tag("SlowGJ")

  it should "compile and run GJ 1-5 & 8-15" taggedAs SlowGJ in {
    // 1-5
    CompileHelpers.assertOutputs("progs/job/gj/1a.sdql", "src/test/job/results/1a.result")
    CompileHelpers.assertOutputs("progs/job/gj/1b.sdql", "src/test/job/results/1b.result")
    CompileHelpers.assertOutputs("progs/job/gj/1c.sdql", "src/test/job/results/1c.result")
    CompileHelpers.assertOutputs("progs/job/gj/1d.sdql", "src/test/job/results/1d.result")

    CompileHelpers.assertOutputs("progs/job/gj/2a.sdql", "src/test/job/results/2a.result")
    CompileHelpers.assertOutputs("progs/job/gj/2b.sdql", "src/test/job/results/2b.result")
    CompileHelpers.assertOutputs("progs/job/gj/2d.sdql", "src/test/job/results/2d.result")

    CompileHelpers.assertOutputs("progs/job/gj/3a.sdql", "src/test/job/results/3a.result")
    CompileHelpers.assertOutputs("progs/job/gj/3b.sdql", "src/test/job/results/3b.result")
    CompileHelpers.assertOutputs("progs/job/gj/3c.sdql", "src/test/job/results/3c.result")

    CompileHelpers.assertOutputs("progs/job/gj/4a.sdql", "src/test/job/results/4a.result")
    CompileHelpers.assertOutputs("progs/job/gj/4b.sdql", "src/test/job/results/4b.result")
    CompileHelpers.assertOutputs("progs/job/gj/4c.sdql", "src/test/job/results/4c.result")

    CompileHelpers.assertOutputs("progs/job/gj/5c.sdql", "src/test/job/results/5c.result")

    // 8-15
    CompileHelpers.assertOutputs("progs/job/gj/8a.sdql", "src/test/job/results/8a.result")
    CompileHelpers.assertOutputs("progs/job/gj/8b.sdql", "src/test/job/results/8b.result")
    CompileHelpers.assertOutputs("progs/job/gj/8c.sdql", "src/test/job/results/8c.result")
    CompileHelpers.assertOutputs("progs/job/gj/8d.sdql", "src/test/job/results/8d.result")

    CompileHelpers.assertOutputs("progs/job/gj/9a.sdql", "src/test/job/results/9a.result")
    CompileHelpers.assertOutputs("progs/job/gj/9b.sdql", "src/test/job/results/9b.result")
    CompileHelpers.assertOutputs("progs/job/gj/9c.sdql", "src/test/job/results/9c.result")
    CompileHelpers.assertOutputs("progs/job/gj/9d.sdql", "src/test/job/results/9d.result")

    CompileHelpers.assertOutputs("progs/job/gj/10a.sdql", "src/test/job/results/10a.result")
    CompileHelpers.assertOutputs("progs/job/gj/10c.sdql", "src/test/job/results/10c.result")

    CompileHelpers.assertOutputs("progs/job/gj/11a.sdql", "src/test/job/results/11a.result")
    CompileHelpers.assertOutputs("progs/job/gj/11b.sdql", "src/test/job/results/11b.result")
    CompileHelpers.assertOutputs("progs/job/gj/11c.sdql", "src/test/job/results/11c.result")
    CompileHelpers.assertOutputs("progs/job/gj/11d.sdql", "src/test/job/results/11d.result")

    CompileHelpers.assertOutputs("progs/job/gj/12a.sdql", "src/test/job/results/12a.result")
    CompileHelpers.assertOutputs("progs/job/gj/12b.sdql", "src/test/job/results/12b.result")
    CompileHelpers.assertOutputs("progs/job/gj/12c.sdql", "src/test/job/results/12c.result")

    CompileHelpers.assertOutputs("progs/job/gj/13b.sdql", "src/test/job/results/13b.result")
    CompileHelpers.assertOutputs("progs/job/gj/13c.sdql", "src/test/job/results/13c.result")

    CompileHelpers.assertOutputs("progs/job/gj/14a.sdql", "src/test/job/results/14a.result")
    CompileHelpers.assertOutputs("progs/job/gj/14b.sdql", "src/test/job/results/14b.result")
    CompileHelpers.assertOutputs("progs/job/gj/14c.sdql", "src/test/job/results/14c.result")

    CompileHelpers.assertOutputs("progs/job/gj/15a.sdql", "src/test/job/results/15a.result")
    CompileHelpers.assertOutputs("progs/job/gj/15b.sdql", "src/test/job/results/15b.result")
    CompileHelpers.assertOutputs("progs/job/gj/15c.sdql", "src/test/job/results/15c.result")
    CompileHelpers.assertOutputs("progs/job/gj/15d.sdql", "src/test/job/results/15d.result")
  }

  it should "compile and run GJ 6-7" taggedAs SlowGJ in {
    CompileHelpers.assertOutputs("progs/job/gj/6a.sdql", "src/test/job/results/6a.result")
    CompileHelpers.assertOutputs("progs/job/gj/6b.sdql", "src/test/job/results/6b.result")
    CompileHelpers.assertOutputs("progs/job/gj/6c.sdql", "src/test/job/results/6c.result")
    CompileHelpers.assertOutputs("progs/job/gj/6d.sdql", "src/test/job/results/6d.result")
    CompileHelpers.assertOutputs("progs/job/gj/6e.sdql", "src/test/job/results/6e.result")
    CompileHelpers.assertOutputs("progs/job/gj/6f.sdql", "src/test/job/results/6f.result")

    CompileHelpers.assertOutputs("progs/job/gj/7a.sdql", "src/test/job/results/7a.result")
    CompileHelpers.assertOutputs("progs/job/gj/7b.sdql", "src/test/job/results/7b.result")
    CompileHelpers.assertOutputs("progs/job/gj/7c.sdql", "src/test/job/results/7c.result")
  }

  it should "compile and run GJ 16-17" taggedAs SlowGJ in {
    CompileHelpers.assertOutputs("progs/job/gj/16a.sdql", "src/test/job/results/16a.result")
    CompileHelpers.assertOutputs("progs/job/gj/16b.sdql", "src/test/job/results/16b.result")
    CompileHelpers.assertOutputs("progs/job/gj/16c.sdql", "src/test/job/results/16c.result")
    CompileHelpers.assertOutputs("progs/job/gj/16d.sdql", "src/test/job/results/16d.result")

    CompileHelpers.assertOutputs("progs/job/gj/17a.sdql", "src/test/job/results/17a.result")
    CompileHelpers.assertOutputs("progs/job/gj/17b.sdql", "src/test/job/results/17b.result")
    CompileHelpers.assertOutputs("progs/job/gj/17c.sdql", "src/test/job/results/17c.result")
    CompileHelpers.assertOutputs("progs/job/gj/17d.sdql", "src/test/job/results/17d.result")
    CompileHelpers.assertOutputs("progs/job/gj/17e.sdql", "src/test/job/results/17e.result")
    CompileHelpers.assertOutputs("progs/job/gj/17f.sdql", "src/test/job/results/17f.result")
  }

  it should "compile and run GJ 18-20 & 26" taggedAs SlowGJ in {
    // 18-20
    CompileHelpers.assertOutputs("progs/job/gj/18a.sdql", "src/test/job/results/18a.result")
    CompileHelpers.assertOutputs("progs/job/gj/18b.sdql", "src/test/job/results/18b.result")
    CompileHelpers.assertOutputs("progs/job/gj/18c.sdql", "src/test/job/results/18c.result")

    CompileHelpers.assertOutputs("progs/job/gj/19a.sdql", "src/test/job/results/19a.result")
    CompileHelpers.assertOutputs("progs/job/gj/19b.sdql", "src/test/job/results/19b.result")
    CompileHelpers.assertOutputs("progs/job/gj/19c.sdql", "src/test/job/results/19c.result")
    CompileHelpers.assertOutputs("progs/job/gj/19d.sdql", "src/test/job/results/19d.result")

    CompileHelpers.assertOutputs("progs/job/gj/20a.sdql", "src/test/job/results/20a.result")
    CompileHelpers.assertOutputs("progs/job/gj/20b.sdql", "src/test/job/results/20b.result")
    CompileHelpers.assertOutputs("progs/job/gj/20c.sdql", "src/test/job/results/20c.result")

    // 26
    CompileHelpers.assertOutputs("progs/job/gj/26a.sdql", "src/test/job/results/26a.result")
    CompileHelpers.assertOutputs("progs/job/gj/26b.sdql", "src/test/job/results/26b.result")
    CompileHelpers.assertOutputs("progs/job/gj/26c.sdql", "src/test/job/results/26c.result")
  }

  it should "compile and run GJ 21-25 & 27-33" taggedAs SlowGJ in {
    // 21-25
    CompileHelpers.assertOutputs("progs/job/gj/21a.sdql", "src/test/job/results/21a.result")
    CompileHelpers.assertOutputs("progs/job/gj/21b.sdql", "src/test/job/results/21b.result")
    CompileHelpers.assertOutputs("progs/job/gj/21c.sdql", "src/test/job/results/21c.result")

    CompileHelpers.assertOutputs("progs/job/gj/22a.sdql", "src/test/job/results/22a.result")
    CompileHelpers.assertOutputs("progs/job/gj/22b.sdql", "src/test/job/results/22b.result")
    CompileHelpers.assertOutputs("progs/job/gj/22c.sdql", "src/test/job/results/22c.result")
    CompileHelpers.assertOutputs("progs/job/gj/22d.sdql", "src/test/job/results/22d.result")

    CompileHelpers.assertOutputs("progs/job/gj/23a.sdql", "src/test/job/results/23a.result")
    CompileHelpers.assertOutputs("progs/job/gj/23b.sdql", "src/test/job/results/23b.result")
    CompileHelpers.assertOutputs("progs/job/gj/23c.sdql", "src/test/job/results/23c.result")

    CompileHelpers.assertOutputs("progs/job/gj/24a.sdql", "src/test/job/results/24a.result")
    CompileHelpers.assertOutputs("progs/job/gj/24b.sdql", "src/test/job/results/24b.result")

    CompileHelpers.assertOutputs("progs/job/gj/25a.sdql", "src/test/job/results/25a.result")
    CompileHelpers.assertOutputs("progs/job/gj/25b.sdql", "src/test/job/results/25b.result")
    CompileHelpers.assertOutputs("progs/job/gj/25c.sdql", "src/test/job/results/25c.result")

    // 27-33
    CompileHelpers.assertOutputs("progs/job/gj/27a.sdql", "src/test/job/results/27a.result")
    CompileHelpers.assertOutputs("progs/job/gj/27b.sdql", "src/test/job/results/27b.result")
    CompileHelpers.assertOutputs("progs/job/gj/27c.sdql", "src/test/job/results/27c.result")

    CompileHelpers.assertOutputs("progs/job/gj/28a.sdql", "src/test/job/results/28a.result")
    CompileHelpers.assertOutputs("progs/job/gj/28b.sdql", "src/test/job/results/28b.result")
    CompileHelpers.assertOutputs("progs/job/gj/28c.sdql", "src/test/job/results/28c.result")

    CompileHelpers.assertOutputs("progs/job/gj/29a.sdql", "src/test/job/results/29a.result")
    CompileHelpers.assertOutputs("progs/job/gj/29b.sdql", "src/test/job/results/29b.result")
    CompileHelpers.assertOutputs("progs/job/gj/29c.sdql", "src/test/job/results/29c.result")

    CompileHelpers.assertOutputs("progs/job/gj/30a.sdql", "src/test/job/results/30a.result")
    CompileHelpers.assertOutputs("progs/job/gj/30b.sdql", "src/test/job/results/30b.result")
    CompileHelpers.assertOutputs("progs/job/gj/30c.sdql", "src/test/job/results/30c.result")

    CompileHelpers.assertOutputs("progs/job/gj/31a.sdql", "src/test/job/results/31a.result")
    CompileHelpers.assertOutputs("progs/job/gj/31b.sdql", "src/test/job/results/31b.result")
    CompileHelpers.assertOutputs("progs/job/gj/31c.sdql", "src/test/job/results/31c.result")

    CompileHelpers.assertOutputs("progs/job/gj/32b.sdql", "src/test/job/results/32b.result")

    CompileHelpers.assertOutputs("progs/job/gj/33a.sdql", "src/test/job/results/33a.result")
    CompileHelpers.assertOutputs("progs/job/gj/33b.sdql", "src/test/job/results/33b.result")
    CompileHelpers.assertOutputs("progs/job/gj/33c.sdql", "src/test/job/results/33c.result")
  }
}

// run these tests on pre-processed datasets in datasets/job
// manually load balanced - based on GJ queries above
// DON'T run them in parallel - unless you have ~64GB of ram
class CppCompileTestFJ extends AnyFlatSpec with ParallelTestExecution {

  object SlowFJ extends Tag("SlowFJ")

  it should "compile and run FJ 1-5 & 8-15" taggedAs SlowFJ in {
    // 1-5
    CompileHelpers.assertOutputs("progs/job/fj/1a.sdql", "src/test/job/results/1a.result")
    CompileHelpers.assertOutputs("progs/job/fj/1b.sdql", "src/test/job/results/1b.result")
    CompileHelpers.assertOutputs("progs/job/fj/1c.sdql", "src/test/job/results/1c.result")
    CompileHelpers.assertOutputs("progs/job/fj/1d.sdql", "src/test/job/results/1d.result")

    CompileHelpers.assertOutputs("progs/job/fj/2a.sdql", "src/test/job/results/2a.result")
    CompileHelpers.assertOutputs("progs/job/fj/2b.sdql", "src/test/job/results/2b.result")
    CompileHelpers.assertOutputs("progs/job/fj/2d.sdql", "src/test/job/results/2d.result")

    CompileHelpers.assertOutputs("progs/job/fj/3a.sdql", "src/test/job/results/3a.result")
    CompileHelpers.assertOutputs("progs/job/fj/3b.sdql", "src/test/job/results/3b.result")
    CompileHelpers.assertOutputs("progs/job/fj/3c.sdql", "src/test/job/results/3c.result")

    CompileHelpers.assertOutputs("progs/job/fj/4a.sdql", "src/test/job/results/4a.result")
    CompileHelpers.assertOutputs("progs/job/fj/4b.sdql", "src/test/job/results/4b.result")
    CompileHelpers.assertOutputs("progs/job/fj/4c.sdql", "src/test/job/results/4c.result")

    CompileHelpers.assertOutputs("progs/job/fj/5c.sdql", "src/test/job/results/5c.result")

    // 8-15
    CompileHelpers.assertOutputs("progs/job/fj/8a.sdql", "src/test/job/results/8a.result")
    CompileHelpers.assertOutputs("progs/job/fj/8b.sdql", "src/test/job/results/8b.result")
    CompileHelpers.assertOutputs("progs/job/fj/8c.sdql", "src/test/job/results/8c.result")
    CompileHelpers.assertOutputs("progs/job/fj/8d.sdql", "src/test/job/results/8d.result")

    CompileHelpers.assertOutputs("progs/job/fj/9a.sdql", "src/test/job/results/9a.result")
    CompileHelpers.assertOutputs("progs/job/fj/9b.sdql", "src/test/job/results/9b.result")
    CompileHelpers.assertOutputs("progs/job/fj/9c.sdql", "src/test/job/results/9c.result")
    CompileHelpers.assertOutputs("progs/job/fj/9d.sdql", "src/test/job/results/9d.result")

    CompileHelpers.assertOutputs("progs/job/fj/10a.sdql", "src/test/job/results/10a.result")
    CompileHelpers.assertOutputs("progs/job/fj/10c.sdql", "src/test/job/results/10c.result")

    CompileHelpers.assertOutputs("progs/job/fj/11a.sdql", "src/test/job/results/11a.result")
    CompileHelpers.assertOutputs("progs/job/fj/11b.sdql", "src/test/job/results/11b.result")
    CompileHelpers.assertOutputs("progs/job/fj/11c.sdql", "src/test/job/results/11c.result")
    CompileHelpers.assertOutputs("progs/job/fj/11d.sdql", "src/test/job/results/11d.result")

    CompileHelpers.assertOutputs("progs/job/fj/12a.sdql", "src/test/job/results/12a.result")
    CompileHelpers.assertOutputs("progs/job/fj/12b.sdql", "src/test/job/results/12b.result")
    CompileHelpers.assertOutputs("progs/job/fj/12c.sdql", "src/test/job/results/12c.result")

    CompileHelpers.assertOutputs("progs/job/fj/13b.sdql", "src/test/job/results/13b.result")
    CompileHelpers.assertOutputs("progs/job/fj/13c.sdql", "src/test/job/results/13c.result")

    CompileHelpers.assertOutputs("progs/job/fj/14a.sdql", "src/test/job/results/14a.result")
    CompileHelpers.assertOutputs("progs/job/fj/14b.sdql", "src/test/job/results/14b.result")
    CompileHelpers.assertOutputs("progs/job/fj/14c.sdql", "src/test/job/results/14c.result")

    CompileHelpers.assertOutputs("progs/job/fj/15a.sdql", "src/test/job/results/15a.result")
    CompileHelpers.assertOutputs("progs/job/fj/15b.sdql", "src/test/job/results/15b.result")
    CompileHelpers.assertOutputs("progs/job/fj/15c.sdql", "src/test/job/results/15c.result")
    CompileHelpers.assertOutputs("progs/job/fj/15d.sdql", "src/test/job/results/15d.result")
  }

  it should "compile and run FJ 6-7" taggedAs SlowFJ in {
    CompileHelpers.assertOutputs("progs/job/fj/6a.sdql", "src/test/job/results/6a.result")
    CompileHelpers.assertOutputs("progs/job/fj/6b.sdql", "src/test/job/results/6b.result")
    CompileHelpers.assertOutputs("progs/job/fj/6c.sdql", "src/test/job/results/6c.result")
    CompileHelpers.assertOutputs("progs/job/fj/6d.sdql", "src/test/job/results/6d.result")
    CompileHelpers.assertOutputs("progs/job/fj/6e.sdql", "src/test/job/results/6e.result")
    CompileHelpers.assertOutputs("progs/job/fj/6f.sdql", "src/test/job/results/6f.result")

    CompileHelpers.assertOutputs("progs/job/fj/7a.sdql", "src/test/job/results/7a.result")
    CompileHelpers.assertOutputs("progs/job/fj/7b.sdql", "src/test/job/results/7b.result")
    CompileHelpers.assertOutputs("progs/job/fj/7c.sdql", "src/test/job/results/7c.result")
  }

  it should "compile and run FJ 16-17" taggedAs SlowFJ in {
    CompileHelpers.assertOutputs("progs/job/fj/16a.sdql", "src/test/job/results/16a.result")
    CompileHelpers.assertOutputs("progs/job/fj/16b.sdql", "src/test/job/results/16b.result")
    CompileHelpers.assertOutputs("progs/job/fj/16c.sdql", "src/test/job/results/16c.result")
    CompileHelpers.assertOutputs("progs/job/fj/16d.sdql", "src/test/job/results/16d.result")

    CompileHelpers.assertOutputs("progs/job/fj/17a.sdql", "src/test/job/results/17a.result")
    CompileHelpers.assertOutputs("progs/job/fj/17b.sdql", "src/test/job/results/17b.result")
    CompileHelpers.assertOutputs("progs/job/fj/17c.sdql", "src/test/job/results/17c.result")
    CompileHelpers.assertOutputs("progs/job/fj/17d.sdql", "src/test/job/results/17d.result")
    CompileHelpers.assertOutputs("progs/job/fj/17e.sdql", "src/test/job/results/17e.result")
    CompileHelpers.assertOutputs("progs/job/fj/17f.sdql", "src/test/job/results/17f.result")
  }

  it should "compile and run FJ 18-20 & 26" taggedAs SlowFJ in {
    // 18-20
    CompileHelpers.assertOutputs("progs/job/fj/18a.sdql", "src/test/job/results/18a.result")
    CompileHelpers.assertOutputs("progs/job/fj/18b.sdql", "src/test/job/results/18b.result")
    CompileHelpers.assertOutputs("progs/job/fj/18c.sdql", "src/test/job/results/18c.result")

    CompileHelpers.assertOutputs("progs/job/fj/19a.sdql", "src/test/job/results/19a.result")
    CompileHelpers.assertOutputs("progs/job/fj/19b.sdql", "src/test/job/results/19b.result")
    CompileHelpers.assertOutputs("progs/job/fj/19c.sdql", "src/test/job/results/19c.result")
    CompileHelpers.assertOutputs("progs/job/fj/19d.sdql", "src/test/job/results/19d.result")

    CompileHelpers.assertOutputs("progs/job/fj/20a.sdql", "src/test/job/results/20a.result")
    CompileHelpers.assertOutputs("progs/job/fj/20b.sdql", "src/test/job/results/20b.result")
    CompileHelpers.assertOutputs("progs/job/fj/20c.sdql", "src/test/job/results/20c.result")

    // 26
    CompileHelpers.assertOutputs("progs/job/fj/26a.sdql", "src/test/job/results/26a.result")
    CompileHelpers.assertOutputs("progs/job/fj/26b.sdql", "src/test/job/results/26b.result")
    CompileHelpers.assertOutputs("progs/job/fj/26c.sdql", "src/test/job/results/26c.result")
  }

  it should "compile and run FJ 21-25 & 27-33" taggedAs SlowFJ in {
    // 21-25
    CompileHelpers.assertOutputs("progs/job/fj/21a.sdql", "src/test/job/results/21a.result")
    CompileHelpers.assertOutputs("progs/job/fj/21b.sdql", "src/test/job/results/21b.result")
    CompileHelpers.assertOutputs("progs/job/fj/21c.sdql", "src/test/job/results/21c.result")

    CompileHelpers.assertOutputs("progs/job/fj/22a.sdql", "src/test/job/results/22a.result")
    CompileHelpers.assertOutputs("progs/job/fj/22b.sdql", "src/test/job/results/22b.result")
    CompileHelpers.assertOutputs("progs/job/fj/22c.sdql", "src/test/job/results/22c.result")
    CompileHelpers.assertOutputs("progs/job/fj/22d.sdql", "src/test/job/results/22d.result")

    CompileHelpers.assertOutputs("progs/job/fj/23a.sdql", "src/test/job/results/23a.result")
    CompileHelpers.assertOutputs("progs/job/fj/23b.sdql", "src/test/job/results/23b.result")
    CompileHelpers.assertOutputs("progs/job/fj/23c.sdql", "src/test/job/results/23c.result")

    CompileHelpers.assertOutputs("progs/job/fj/24a.sdql", "src/test/job/results/24a.result")
    CompileHelpers.assertOutputs("progs/job/fj/24b.sdql", "src/test/job/results/24b.result")

    CompileHelpers.assertOutputs("progs/job/fj/25a.sdql", "src/test/job/results/25a.result")
    CompileHelpers.assertOutputs("progs/job/fj/25b.sdql", "src/test/job/results/25b.result")
    CompileHelpers.assertOutputs("progs/job/fj/25c.sdql", "src/test/job/results/25c.result")

    // 27-33
    CompileHelpers.assertOutputs("progs/job/fj/27a.sdql", "src/test/job/results/27a.result")
    CompileHelpers.assertOutputs("progs/job/fj/27b.sdql", "src/test/job/results/27b.result")
    CompileHelpers.assertOutputs("progs/job/fj/27c.sdql", "src/test/job/results/27c.result")

    CompileHelpers.assertOutputs("progs/job/fj/28a.sdql", "src/test/job/results/28a.result")
    CompileHelpers.assertOutputs("progs/job/fj/28b.sdql", "src/test/job/results/28b.result")
    CompileHelpers.assertOutputs("progs/job/fj/28c.sdql", "src/test/job/results/28c.result")

    CompileHelpers.assertOutputs("progs/job/fj/29a.sdql", "src/test/job/results/29a.result")
    CompileHelpers.assertOutputs("progs/job/fj/29b.sdql", "src/test/job/results/29b.result")
    CompileHelpers.assertOutputs("progs/job/fj/29c.sdql", "src/test/job/results/29c.result")

    CompileHelpers.assertOutputs("progs/job/fj/30a.sdql", "src/test/job/results/30a.result")
    CompileHelpers.assertOutputs("progs/job/fj/30b.sdql", "src/test/job/results/30b.result")
    CompileHelpers.assertOutputs("progs/job/fj/30c.sdql", "src/test/job/results/30c.result")

    CompileHelpers.assertOutputs("progs/job/fj/31a.sdql", "src/test/job/results/31a.result")
    CompileHelpers.assertOutputs("progs/job/fj/31b.sdql", "src/test/job/results/31b.result")
    CompileHelpers.assertOutputs("progs/job/fj/31c.sdql", "src/test/job/results/31c.result")

    CompileHelpers.assertOutputs("progs/job/fj/32b.sdql", "src/test/job/results/32b.result")

    CompileHelpers.assertOutputs("progs/job/fj/33a.sdql", "src/test/job/results/33a.result")
    CompileHelpers.assertOutputs("progs/job/fj/33b.sdql", "src/test/job/results/33b.result")
    CompileHelpers.assertOutputs("progs/job/fj/33c.sdql", "src/test/job/results/33c.result")
  }
}

object CompileHelpers {
  def assertOutputs(sdqlPath: String, outPath: String, sf: String = ""): Unit = {
    val source_code = SourceCode.fromFile(sdqlPath, if (sf.isEmpty) identity else patch(sf))
    val cpp         = CppCodegen(source_code.exp)
    val actual      = compile(sdqlPath, cpp)
    val source      = scala.io.Source.fromFile(outPath)
    val expected = try source.mkString
    finally source.close()
    assert(actual.trim == expected.trim, s"\n${actual.trim}\n!=\n${expected.trim}")
  }
  private def patch(sf: String)(s: String) = s.replace("datasets/tpch/", s"src/test/tpch/data/SF_$sf/")
}
