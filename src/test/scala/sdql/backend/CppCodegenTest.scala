package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.backend.CppCompile.{clangCmd, inGeneratedDir}
import sdql.frontend.{Interpolator, SourceCode}
import sdql.ir.{Exp, RecordValue}

// comprehensive subset of tests from the parser useful for TPCH
class CppCodegenTest extends AnyFlatSpec with ParallelTestExecution {

  it should "codegen constant true" in {
    compilesExp(sdql"true")
  }
  it should "codegen constant false" in {
    compilesExp(sdql"false")
  }
  it should "codegen constant int" in {
    compilesExp(sdql"42")
  }
  it should "codegen constant real" in {
    compilesExp(sdql"42.2")
  }
  it should "codegen constant string" in {
    compilesExp(sdql""" "foo" """)
  }
  it should "codegen constant date" in {
    compilesExp(sdql"date(19700101)")
  }
  it should "codegen constant tuple" in {
    compilesExp(sdql"< a = 1, b = 2 >")
  }
  it should "codegen constant map" in {
    compilesExp(sdql"""{ "a" -> 1, "b" -> 2 }""")
  }
  it should "codegen constant map requiring type promotion" in {
    val e = sdql"""{ "a" -> 1, "b" -> 2.5 }"""
    import sdql.analysis.TypeInference
    import sdql.ir.{DictType, RealType, StringType}
    assert(TypeInference(e) == DictType(StringType(), RealType))
    compilesExp(e)
  }

  it should "codegen arith op *" in {
    compilesExp(sdql"1 * 2")
  }
  it should "codegen arith op +" in {
    compilesExp(sdql"1 + 2")
  }
  it should "codegen arith op -" in {
    compilesExp(sdql"1 - 2")
  }
  it should "codegen arith op ^" in {
    compilesExp(sdql"3 ^ 2")
  }
  it should "codegen arith op /" in {
    compilesExp(sdql"42 / 21")
  }

  it should "codegen logical op &&" in {
    compilesExp(sdql"true && true")
    compilesExp(sdql"true && false")
    compilesExp(sdql"false && true")
    compilesExp(sdql"false && false")
  }
  it should "codegen logical op ||" in {
    compilesExp(sdql"true || true")
    compilesExp(sdql"true || false")
    compilesExp(sdql"false || true")
    compilesExp(sdql"false || false")
  }
  it should "codegen logical ops" in {
    compilesExp(sdql"(3 < 2) && (3 < 4)")
    compilesExp(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.b < x.c))")
    compilesExp(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.c < x.b))")
  }

  it should "codegen comparisons on bool" in {
    compilesExp(sdql"true == true")
    compilesExp(sdql"true == false")
    compilesExp(sdql"false != true")
    compilesExp(sdql"false == false")
  }

  it should "codegen records" in {
    compilesExp(sdql"< a=1, b=1.5 >")
    compilesExp(sdql"concat(< a=1 >, < b=1.5 >)")
    compilesExp(sdql"concat(< a=1, b=1.5 >, < b=1.5 >)")
    assertThrows[Exception] {
      compilesExp(sdql"concat(< a=1, b=2.5 >, < b=1.5 >)")
    }
  }

  private val iList = 0 until 10
  private val sList = 100 until 110
  private val sRel = for(i <- iList; s <- sList) yield (i, s, i * s + 42)
  private val rRel = for(s <- sList) yield (s, s - 42)
  private val s = {
    sRel.map(e => RecordValue(Seq("i" -> e._1, "s" -> e._2, "u" -> e._3)) -> 1).toMap
  }
  private val r = {
    rRel.map(e => RecordValue(Seq("s" -> e._1, "c" -> e._2)) -> 1).toMap
  }

  it should "codegen sums" in {
    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s_v ")
    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s ")
    compilesExp(sdql"let S = $s in sum(<s, s_v> <- S) s_v ")
    compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
      let S1 = sum(<s, s_v> <- S) if(s == 1) then {s -> s_v*3} else {}
      sum(<s, s_v> <- S1) s_v
      """)
    compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
      let S1 = sum(<s, s_v> <- S) if(s == 3) then {s -> s_v*3} else {}
      sum(<s, s_v> <- S1) s_v
      """)
    compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> -1.5 }
      sum(<s, s_v> <- S) {1 -> s_v}
      """)
  }

  it should "codegen joins" in {
    compilesExp(sdql"""
let S = $s in
let R = $r in
let H_R = sum(<x_r, x_r_v> <- R)
  { < s = x_r.s > -> { < c = x_r.c > -> R(x_r) } }
sum(<x_s, x_s_v> <- S)
  let R_p = H_R(<s = x_s.s>)
  sum(<x_r, x_r_v> <- R_p)
    { < i = x_s.i, s = x_s.s, c = x_r.c > -> R_p(x_r) * S(x_s) }
""")
    compilesExp(sdql"""
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
    compilesExp(sdql"""let Nodes = {
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

  it should "codegen TPCH Q1" in {
    compilesFile("progs/tpch/q1.sdql")
  }
  it should "codegen TPCH Q2" in {
    compilesFile("progs/tpch/q2.sdql")
  }
  it should "codegen TPCH Q3" in {
    compilesFile("progs/tpch/q3.sdql")
  }
  it should "codegen TPCH Q4" in {
    compilesFile("progs/tpch/q4.sdql")
  }
  it should "codegen TPCH Q5" in {
    compilesFile("progs/tpch/q5.sdql")
  }
  it should "codegen TPCH Q6" in {
    compilesFile("progs/tpch/q6.sdql")
  }
  it should "codegen TPCH Q7" in {
    compilesFile("progs/tpch/q7.sdql")
  }
  it should "codegen TPCH Q8" in {
    compilesFile("progs/tpch/q8.sdql")
  }
  it should "codegen TPCH Q9" in {
    compilesFile("progs/tpch/q9.sdql")
  }
  it should "codegen TPCH Q10" in {
    compilesFile("progs/tpch/q10.sdql")
  }
  it should "codegen TPCH Q11" in {
    compilesFile("progs/tpch/q11.sdql")
  }
  it should "codegen TPCH Q12" in {
    compilesFile("progs/tpch/q12.sdql")
  }
  it should "codegen TPCH Q13" in {
    compilesFile("progs/tpch/q13.sdql")
  }
  it should "codegen TPCH Q14" in {
    compilesFile("progs/tpch/q14.sdql")
  }
  it should "codegen TPCH Q15" in {
    compilesFile("progs/tpch/q15.sdql")
  }
  it should "codegen TPCH Q16" in {
    compilesFile("progs/tpch/q16.sdql")
  }
  it should "codegen TPCH Q17" in {
    compilesFile("progs/tpch/q17.sdql")
  }
  it should "codegen TPCH Q18" in {
    compilesFile("progs/tpch/q18.sdql")
  }
  it should "codegen TPCH Q19" in {
    compilesFile("progs/tpch/q19.sdql")
  }
  it should "codegen TPCH Q20" in {
    compilesFile("progs/tpch/q20.sdql")
  }
  it should "codegen TPCH Q21" in {
    compilesFile("progs/tpch/q21.sdql")
  }
  it should "codegen TPCH Q22" in {
    compilesFile("progs/tpch/q22.sdql")
  }

  it should "codegen job 1a" in {
    compilesFile("progs/job/gj/1a.sdql")
  }
  it should "codegen job 3a" in {
    compilesFile("progs/job/gj/3a.sdql")
  }

  private def compilesFile(path: String) = compilesExp(SourceCode.fromFile(path).exp)
  private def compilesExp(e: Exp) = assert(fromCpp(CppCodegen(e)) == 0)
  private def fromCpp(cpp: String) = inGeneratedDir(Seq("bash", "-c", cmd(escape(cpp)))).run().exitValue()
  private def cmd(cpp: String) = s"${clangCmd.mkString(" ")} -xc++ -fsyntax-only - <<< '$cpp'"
  // silly hack - escape single quotes in C++ source code so we can pass it to bash
  private def escape(cpp: String) = cpp.replace(singleQuote.toString, s"char(${singleQuote.toInt})")
  private val singleQuote = '\''
}
