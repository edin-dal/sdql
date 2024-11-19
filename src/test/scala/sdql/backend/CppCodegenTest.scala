package sdql.backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.backend.CppCompile.{ clangCmd, inGeneratedDir }
import sdql.frontend.{ Interpolator, SourceCode }
import sdql.ir.{ Exp, RecordValue }
import sdql.transformations.Rewriter

// comprehensive subset of tests from the interpreter useful for TPCH
class CppCodegenTest extends AnyFlatSpec with ParallelTestExecution {

  it should "codegen constant true" in CodegenHelpers.compilesExp(sdql"true")
  it should "codegen constant false" in CodegenHelpers.compilesExp(sdql"false")
  it should "codegen constant int" in CodegenHelpers.compilesExp(sdql"42")
  it should "codegen constant real" in CodegenHelpers.compilesExp(sdql"42.2")
  it should "codegen constant string" in CodegenHelpers.compilesExp(sdql""" "foo" """)
  it should "codegen constant date" in CodegenHelpers.compilesExp(sdql"date(19700101)")
  it should "codegen constant tuple" in CodegenHelpers.compilesExp(sdql"< a = 1, b = 2 >")
  it should "codegen constant map" in CodegenHelpers.compilesExp(sdql"""{ "a" -> 1, "b" -> 2 }""")
  it should "codegen constant map requiring type promotion" in {
    val e = sdql"""{ "a" -> 1, "b" -> 2.5 }"""
    import sdql.analysis.TypeInference
    import sdql.ir.{ DictType, RealType, StringType }
    assert(TypeInference(e) == DictType(StringType(), RealType))
    CodegenHelpers.compilesExp(e)
  }

  it should "codegen arith op *" in CodegenHelpers.compilesExp(sdql"1 * 2")
  it should "codegen arith op +" in CodegenHelpers.compilesExp(sdql"1 + 2")
  it should "codegen arith op -" in CodegenHelpers.compilesExp(sdql"1 - 2")
  it should "codegen arith op ^" in CodegenHelpers.compilesExp(sdql"3 ^ 2")
  it should "codegen arith op /" in CodegenHelpers.compilesExp(sdql"42 / 21")

  it should "codegen neg const" in CodegenHelpers.compilesExp(sdql"-2")
  it should "codegen neg expr" in CodegenHelpers.compilesExp(sdql"-(1 + 1)")

  it should "codegen logical op &&" in {
    CodegenHelpers.compilesExp(sdql"true && true")
    CodegenHelpers.compilesExp(sdql"true && false")
    CodegenHelpers.compilesExp(sdql"false && true")
    CodegenHelpers.compilesExp(sdql"false && false")
  }
  it should "codegen logical op ||" in {
    CodegenHelpers.compilesExp(sdql"true || true")
    CodegenHelpers.compilesExp(sdql"true || false")
    CodegenHelpers.compilesExp(sdql"false || true")
    CodegenHelpers.compilesExp(sdql"false || false")
  }
  it should "codegen logical ops" in {
    CodegenHelpers.compilesExp(sdql"(3 < 2) && (3 < 4)")
    CodegenHelpers.compilesExp(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.b < x.c))")
    CodegenHelpers.compilesExp(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.c < x.b))")
  }

  it should "codegen comparisons on bool" in {
    CodegenHelpers.compilesExp(sdql"true == true")
    CodegenHelpers.compilesExp(sdql"true == false")
    CodegenHelpers.compilesExp(sdql"false != true")
    CodegenHelpers.compilesExp(sdql"false == false")
  }

  it should "codegen records" in {
    CodegenHelpers.compilesExp(sdql"< >")
    CodegenHelpers.compilesExp(sdql"< a=1, b=1.5 >")
    CodegenHelpers.compilesExp(sdql"concat(< a=1 >, < b=1.5 >)")
    CodegenHelpers.compilesExp(sdql"concat(< a=1, b=1.5 >, < b=1.5 >)")
    assertThrows[Exception] {
      CodegenHelpers.compilesExp(sdql"concat(< a=1, b=2.5 >, < b=1.5 >)")
    }
  }

  private val iList = 0 until 10
  private val sList = 100 until 110
  private val sRel  = for (i <- iList; s <- sList) yield (i, s, i * s + 42)
  private val rRel  = for (s <- sList) yield (s, s - 42)
  private val s     =
    sRel.map(e => RecordValue(Seq("i" -> e._1, "s" -> e._2, "u" -> e._3)) -> 1).toMap
  private val r     =
    rRel.map(e => RecordValue(Seq("s" -> e._1, "c" -> e._2)) -> 1).toMap

  it should "codegen sums" in {
    CodegenHelpers.compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s_v ")
    CodegenHelpers.compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s ")
    CodegenHelpers.compilesExp(sdql"let S = $s in sum(<s, s_v> <- S) s_v ")
    CodegenHelpers.compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
      let S1 = sum(<s, s_v> <- S) if(s == 1) then {s -> s_v*3} else {}
      sum(<s, s_v> <- S1) s_v
      """)
    CodegenHelpers.compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
      let S1 = sum(<s, s_v> <- S) if(s == 3) then {s -> s_v*3} else {}
      sum(<s, s_v> <- S1) s_v
      """)
    CodegenHelpers.compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> -1.5 }
      sum(<s, s_v> <- S) {1 -> s_v}
      """)
  }

  it should "codegen joins" in {
    CodegenHelpers.compilesExp(sdql"""
let S = $s in
let R = $r in
let H_R = sum(<x_r, x_r_v> <- R)
  { < s = x_r.s > -> { < c = x_r.c > -> R(x_r) } }
sum(<x_s, x_s_v> <- S)
  let R_p = H_R(<s = x_s.s>)
  sum(<x_r, x_r_v> <- R_p)
    { < i = x_s.i, s = x_s.s, c = x_r.c > -> R_p(x_r) * S(x_s) }
""")
    CodegenHelpers.compilesExp(sdql"""
let S = $s in
let R = $r in
sum(<x_s, x_s_v> <- S)
  sum(<x_r, x_r_v> <- R)
    if (x_r.s == x_s.s) then
      { < i = x_s.i, s = x_s.s, c = x_r.c > -> R(x_r) * S(x_s) }
    else
      { }
""")
  }

  it should "codegen simple graph queries" in {
    CodegenHelpers.compilesExp(sdql"""let Nodes = {
      0 -> <label = {"Person" -> true, "Director" -> true, "Singer" -> true}, name="Oliver Stone", age=30>,
      1 -> <label = {"Person" -> true, "Director" -> true}, name="Michael Douglas", age=35>,
      2 -> <label = {"Person" -> true, "Actor" -> true}, name="Charlie Sheen",age=32>}
    sum(<k,v> in Nodes)
      if(v.name=="Charlie Sheen") then
        {<age=v.age> -> 1}
      else
        {}
    """)
  }

  // checks we support columnar layout - though we use row layout in JOB GF/FJ queries
  it should "codegen smallvecdicts" in {
    CodegenHelpers.compilesExp(sdql"""
      let a = { 0 -> 1, 1 -> 10, 2 -> 100 }
      let b = { 0 -> "a", 1 -> "b", 2 -> "c" }
      let c = sum(<i, _> <- range(3)) @smallvecdicts(0) { <a = a(i), b = b(i)> -> 1 }
      sum(<record, _> <- c) <a = record.a, b = record.b>
      """)
  }

  it should "codegen simple promotion" in CodegenHelpers.compilesExp(sdql"promote[double](1)")

  it should "codegen record get" in CodegenHelpers.compilesExp(sdql"let record=<_=2.5> in record(1)")
}

class CppCodegenTestTPCH extends AnyFlatSpec with ParallelTestExecution {
  it should "codegen TPCH Q1" in CodegenHelpers.compilesFile("progs/tpch/q1.sdql")
  it should "codegen TPCH Q2" in CodegenHelpers.compilesFile("progs/tpch/q2.sdql")
  it should "codegen TPCH Q3" in CodegenHelpers.compilesFile("progs/tpch/q3.sdql")
  it should "codegen TPCH Q4" in CodegenHelpers.compilesFile("progs/tpch/q4.sdql")
  it should "codegen TPCH Q5" in CodegenHelpers.compilesFile("progs/tpch/q5.sdql")
  it should "codegen TPCH Q6" in CodegenHelpers.compilesFile("progs/tpch/q6.sdql")
  it should "codegen TPCH Q7" in CodegenHelpers.compilesFile("progs/tpch/q7.sdql")
  it should "codegen TPCH Q8" in CodegenHelpers.compilesFile("progs/tpch/q8.sdql")
  it should "codegen TPCH Q9" in CodegenHelpers.compilesFile("progs/tpch/q9.sdql")
  it should "codegen TPCH Q10" in CodegenHelpers.compilesFile("progs/tpch/q10.sdql")
  it should "codegen TPCH Q11" in CodegenHelpers.compilesFile("progs/tpch/q11.sdql")
  it should "codegen TPCH Q12" in CodegenHelpers.compilesFile("progs/tpch/q12.sdql")
  it should "codegen TPCH Q13" in CodegenHelpers.compilesFile("progs/tpch/q13.sdql")
  it should "codegen TPCH Q14" in CodegenHelpers.compilesFile("progs/tpch/q14.sdql")
  it should "codegen TPCH Q15" in CodegenHelpers.compilesFile("progs/tpch/q15.sdql")
  it should "codegen TPCH Q16" in CodegenHelpers.compilesFile("progs/tpch/q16.sdql")
  it should "codegen TPCH Q17" in CodegenHelpers.compilesFile("progs/tpch/q17.sdql")
  it should "codegen TPCH Q18" in CodegenHelpers.compilesFile("progs/tpch/q18.sdql")
  it should "codegen TPCH Q19" in CodegenHelpers.compilesFile("progs/tpch/q19.sdql")
  it should "codegen TPCH Q20" in CodegenHelpers.compilesFile("progs/tpch/q20.sdql")
  it should "codegen TPCH Q21" in CodegenHelpers.compilesFile("progs/tpch/q21.sdql")
  it should "codegen TPCH Q22" in CodegenHelpers.compilesFile("progs/tpch/q22.sdql")
}

class CppCodegenTestJOBGJ extends AnyFlatSpec with ParallelTestExecution {
  it should "codegen JOB GJ 1" in {
    CodegenHelpers.compilesFile("progs/job/gj/1a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/1b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/1c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/1d.sdql")
  }
  it should "codegen JOB GJ 2" in {
    CodegenHelpers.compilesFile("progs/job/gj/2a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/2b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/2d.sdql")
  }
  it should "codegen JOB GJ 3" in {
    CodegenHelpers.compilesFile("progs/job/gj/3a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/3b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/3c.sdql")
  }
  it should "codegen JOB GJ 4" in {
    CodegenHelpers.compilesFile("progs/job/gj/4a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/4b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/4c.sdql")
  }
  it should "codegen JOB GJ 5" in {
    CodegenHelpers.compilesFile("progs/job/gj/5c.sdql")
  }
  it should "codegen JOB GJ 6" in {
    CodegenHelpers.compilesFile("progs/job/gj/6a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/6b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/6c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/6d.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/6e.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/6f.sdql")
  }
  it should "codegen JOB GJ 7" in {
    CodegenHelpers.compilesFile("progs/job/gj/7a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/7b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/7c.sdql")
  }
  it should "codegen JOB GJ 8" in {
    CodegenHelpers.compilesFile("progs/job/gj/8a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/8b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/8c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/8d.sdql")
  }
  it should "codegen JOB GJ 9" in {
    CodegenHelpers.compilesFile("progs/job/gj/9a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/9b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/9c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/9d.sdql")
  }
  it should "codegen JOB GJ 10" in {
    CodegenHelpers.compilesFile("progs/job/gj/10a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/10c.sdql")
  }
  it should "codegen JOB GJ 11" in {
    CodegenHelpers.compilesFile("progs/job/gj/11a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/11b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/11c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/11d.sdql")
  }
  it should "codegen JOB GJ 12" in {
    CodegenHelpers.compilesFile("progs/job/gj/12a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/12b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/12c.sdql")
  }
  it should "codegen JOB GJ 13" in {
    CodegenHelpers.compilesFile("progs/job/gj/13a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/13b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/13c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/13d.sdql")
  }
  it should "codegen JOB GJ 14" in {
    CodegenHelpers.compilesFile("progs/job/gj/14a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/14b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/14c.sdql")
  }
  it should "codegen JOB GJ 15" in {
    CodegenHelpers.compilesFile("progs/job/gj/15a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/15b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/15c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/15d.sdql")
  }
  it should "codegen JOB GJ 16" in {
    CodegenHelpers.compilesFile("progs/job/gj/16a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/16b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/16c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/16d.sdql")
  }
  it should "codegen JOB GJ 17" in {
    CodegenHelpers.compilesFile("progs/job/gj/17a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/17b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/17c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/17d.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/17e.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/17f.sdql")
  }
  it should "codegen JOB GJ 18" in {
    CodegenHelpers.compilesFile("progs/job/gj/18a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/18b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/18c.sdql")
  }
  it should "codegen JOB GJ 19" in {
    CodegenHelpers.compilesFile("progs/job/gj/19a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/19b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/19c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/19d.sdql")
  }
  it should "codegen JOB GJ 20" in {
    CodegenHelpers.compilesFile("progs/job/gj/20a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/20b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/20c.sdql")
  }
  it should "codegen JOB GJ 21" in {
    CodegenHelpers.compilesFile("progs/job/gj/21a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/21b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/21c.sdql")
  }
  it should "codegen JOB GJ 22" in {
    CodegenHelpers.compilesFile("progs/job/gj/22a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/22b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/22c.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/22d.sdql")
  }
  it should "codegen JOB GJ 23" in {
    CodegenHelpers.compilesFile("progs/job/gj/23a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/23b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/23c.sdql")
  }
  it should "codegen JOB GJ 24" in {
    CodegenHelpers.compilesFile("progs/job/gj/24a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/24b.sdql")
  }
  it should "codegen JOB GJ 25" in {
    CodegenHelpers.compilesFile("progs/job/gj/25a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/25b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/25c.sdql")
  }
  it should "codegen JOB GJ 26" in {
    CodegenHelpers.compilesFile("progs/job/gj/26a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/26b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/26c.sdql")
  }
  it should "codegen JOB GJ 27" in {
    CodegenHelpers.compilesFile("progs/job/gj/27a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/27b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/27c.sdql")
  }
  it should "codegen JOB GJ 28" in {
    CodegenHelpers.compilesFile("progs/job/gj/28a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/28b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/28c.sdql")
  }
  it should "codegen JOB GJ 29" in {
    CodegenHelpers.compilesFile("progs/job/gj/29a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/29b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/29c.sdql")
  }
  it should "codegen JOB GJ 30" in {
    CodegenHelpers.compilesFile("progs/job/gj/30a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/30b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/30c.sdql")
  }
  it should "codegen JOB GJ 31" in {
    CodegenHelpers.compilesFile("progs/job/gj/31a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/31b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/31c.sdql")
  }
  it should "codegen JOB GJ 32" in {
    CodegenHelpers.compilesFile("progs/job/gj/32b.sdql")
  }
  it should "codegen JOB GJ 33" in {
    CodegenHelpers.compilesFile("progs/job/gj/33a.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/33b.sdql")
    CodegenHelpers.compilesFile("progs/job/gj/33c.sdql")
  }
}

class CppCodegenTestJOBFJ extends AnyFlatSpec with ParallelTestExecution {
  it should "codegen JOB FJ 1" in {
    CodegenHelpers.compilesFile("progs/job/fj/1a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/1b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/1c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/1d.sdql")
  }
  it should "codegen JOB FJ 2" in {
    CodegenHelpers.compilesFile("progs/job/fj/2a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/2b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/2d.sdql")
  }
  it should "codegen JOB FJ 3" in {
    CodegenHelpers.compilesFile("progs/job/fj/3a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/3b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/3c.sdql")
  }
  it should "codegen JOB FJ 4" in {
    CodegenHelpers.compilesFile("progs/job/fj/4a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/4b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/4c.sdql")
  }
  it should "codegen JOB FJ 5" in {
    CodegenHelpers.compilesFile("progs/job/fj/5c.sdql")
  }
  it should "codegen JOB FJ 6" in {
    CodegenHelpers.compilesFile("progs/job/fj/6a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/6b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/6c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/6d.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/6e.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/6f.sdql")
  }
  it should "codegen JOB FJ 7" in {
    CodegenHelpers.compilesFile("progs/job/fj/7a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/7b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/7c.sdql")
  }
  it should "codegen JOB FJ 8" in {
    CodegenHelpers.compilesFile("progs/job/fj/8a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/8b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/8c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/8d.sdql")
  }
  it should "codegen JOB FJ 9" in {
    CodegenHelpers.compilesFile("progs/job/fj/9a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/9b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/9c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/9d.sdql")
  }
  it should "codegen JOB FJ 10" in {
    CodegenHelpers.compilesFile("progs/job/fj/10a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/10c.sdql")
  }
  it should "codegen JOB FJ 11" in {
    CodegenHelpers.compilesFile("progs/job/fj/11a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/11b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/11c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/11d.sdql")
  }
  it should "codegen JOB FJ 12" in {
    CodegenHelpers.compilesFile("progs/job/fj/12a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/12b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/12c.sdql")
  }
  it should "codegen JOB FJ 13" in {
    CodegenHelpers.compilesFile("progs/job/fj/13a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/13b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/13c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/13d.sdql")
  }
  it should "codegen JOB FJ 14" in {
    CodegenHelpers.compilesFile("progs/job/fj/14a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/14b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/14c.sdql")
  }
  it should "codegen JOB FJ 15" in {
    CodegenHelpers.compilesFile("progs/job/fj/15a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/15b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/15c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/15d.sdql")
  }
  it should "codegen JOB FJ 16" in {
    CodegenHelpers.compilesFile("progs/job/fj/16a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/16b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/16c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/16d.sdql")
  }
  it should "codegen JOB FJ 17" in {
    CodegenHelpers.compilesFile("progs/job/fj/17a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/17b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/17c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/17d.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/17e.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/17f.sdql")
  }
  it should "codegen JOB FJ 18" in {
    CodegenHelpers.compilesFile("progs/job/fj/18a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/18b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/18c.sdql")
  }
  it should "codegen JOB FJ 19" in {
    CodegenHelpers.compilesFile("progs/job/fj/19a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/19b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/19c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/19d.sdql")
  }
  it should "codegen JOB FJ 20" in {
    CodegenHelpers.compilesFile("progs/job/fj/20a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/20b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/20c.sdql")
  }
  it should "codegen JOB FJ 21" in {
    CodegenHelpers.compilesFile("progs/job/fj/21a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/21b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/21c.sdql")
  }
  it should "codegen JOB FJ 22" in {
    CodegenHelpers.compilesFile("progs/job/fj/22a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/22b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/22c.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/22d.sdql")
  }
  it should "codegen JOB FJ 23" in {
    CodegenHelpers.compilesFile("progs/job/fj/23a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/23b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/23c.sdql")
  }
  it should "codegen JOB FJ 24" in {
    CodegenHelpers.compilesFile("progs/job/fj/24a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/24b.sdql")
  }
  it should "codegen JOB FJ 25" in {
    CodegenHelpers.compilesFile("progs/job/fj/25a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/25b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/25c.sdql")
  }
  it should "codegen JOB FJ 26" in {
    CodegenHelpers.compilesFile("progs/job/fj/26a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/26b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/26c.sdql")
  }
  it should "codegen JOB FJ 27" in {
    CodegenHelpers.compilesFile("progs/job/fj/27a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/27b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/27c.sdql")
  }
  it should "codegen JOB FJ 28" in {
    CodegenHelpers.compilesFile("progs/job/fj/28a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/28b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/28c.sdql")
  }
  it should "codegen JOB FJ 29" in {
    CodegenHelpers.compilesFile("progs/job/fj/29a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/29b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/29c.sdql")
  }
  it should "codegen JOB FJ 30" in {
    CodegenHelpers.compilesFile("progs/job/fj/30a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/30b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/30c.sdql")
  }
  it should "codegen JOB FJ 31" in {
    CodegenHelpers.compilesFile("progs/job/fj/31a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/31b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/31c.sdql")
  }
  it should "codegen JOB FJ 32" in {
    CodegenHelpers.compilesFile("progs/job/fj/32b.sdql")
  }
  it should "codegen JOB FJ 33" in {
    CodegenHelpers.compilesFile("progs/job/fj/33a.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/33b.sdql")
    CodegenHelpers.compilesFile("progs/job/fj/33c.sdql")
  }
}

class CppCodegenTestLSQB extends AnyFlatSpec with ParallelTestExecution {
  it should "codegen LSQB GJ" in {
    CodegenHelpers.compilesFile("progs/lsqb/gj/q1.sdql")
    CodegenHelpers.compilesFile("progs/lsqb/gj/q2.sdql")
    CodegenHelpers.compilesFile("progs/lsqb/gj/q4.sdql")
    CodegenHelpers.compilesFile("progs/lsqb/gj/q5.sdql")
  }
  it should "codegen LSQB FJ" in {
    CodegenHelpers.compilesFile("progs/lsqb/fj/q1.sdql")
    CodegenHelpers.compilesFile("progs/lsqb/fj/q2.sdql")
    CodegenHelpers.compilesFile("progs/lsqb/fj/q4.sdql")
    CodegenHelpers.compilesFile("progs/lsqb/fj/q5.sdql")
  }
}

class CppCodegenTestJOBSorting extends AnyFlatSpec with ParallelTestExecution {
  it should "codegen JOB FJ 3a pure sorting" in CodegenHelpers.compilesFile("progs/sorting/job/fj_pure/3a.sdql")
  it should "codegen JOB FJ 10a pure sorting" in CodegenHelpers.compilesFile("progs/sorting/job/fj_pure/10a.sdql")
  it should "codegen JOB GJ 3a hybrid sorting" in CodegenHelpers.compilesFile("progs/sorting/job/gj_hybrid/3a.sdql")
  it should "codegen JOB GJ 3a hybrid sorting (optimised)" in CodegenHelpers.compilesFile(
    "progs/sorting/job/gj_hybrid/3a_optimised.sdql"
  )
  it should "codegen JOB GJ 13a hybrid sorting" in CodegenHelpers.compilesFile("progs/sorting/job/gj_hybrid/13a.sdql")
  it should "codegen JOB GJ 13a hybrid sorting (optimised)" in CodegenHelpers.compilesFile(
    "progs/sorting/job/gj_hybrid/13a_optimised.sdql"
  )
  it should "codegen JOB GJ 13b hybrid sorting" in CodegenHelpers.compilesFile("progs/sorting/job/gj_hybrid/13b.sdql")
}

object CodegenHelpers {
  def compilesFile(path: String): Unit = compilesExp(SourceCode.fromFile(path).exp)
  def compilesExp(e: Exp): Unit        = assert(fromCpp(CppCodegen(Rewriter.rewrite(e))) == 0)
  private def fromCpp(cpp: String)     = inGeneratedDir(Seq("bash", "-c", cmd(escape(cpp)))).run().exitValue()
  private def cmd(cpp: String)         = s"${clangCmd.mkString(" ")} -xc++ -fsyntax-only - <<< '$cpp'"
  // silly hack - escape single quotes in C++ source code so we can pass it to bash
  private def escape(cpp: String)      = cpp.replace(singleQuote.toString, s"char(${singleQuote.toInt})")
  private val singleQuote              = '\''
}
