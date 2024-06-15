package sdql
package backend

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.frontend.{Interpolator, SourceCode}
import sdql.ir.{Exp, RecordValue}

class CppCodegenTest extends AnyFlatSpec with ParallelTestExecution {

  it should "compile constant true" in {
    compilesExp(sdql"true")
  }
  it should "compile constant false" in {
    compilesExp(sdql"false")
  }
  it should "compile constant int" in {
    compilesExp(sdql"42")
  }
  it should "compile constant real" in {
    compilesExp(sdql"42.2")
  }
  it should "compile constant string" in {
    compilesExp(sdql""" "foo" """)
  }
  it should "compile constant date" in {
    compilesExp(sdql"date(19700101)")
  }
  it should "compile constant tuple" in {
    compilesExp(sdql"< a = 1, b = 2 >")
  }
  it should "compile constant map" in {
    compilesExp(sdql"""{ "a" -> 1, "b" -> 2 }""")
  }
  // TODO this should be a unit test for type inference
  it should "compile constant map requiring type promotion" in {
    val e = sdql"""{ "a" -> 1, "b" -> 2.5 }"""
    import sdql.analysis.TypeInference
    import sdql.ir.{DictType, RealType, StringType}
    assert(TypeInference(e) == DictType(StringType, RealType))
    compilesExp(e)
  }

  it should "compile arith op *" in {
    compilesExp(sdql"1 * 2")
  }
  it should "compile arith op +" in {
    compilesExp(sdql"1 + 2")
  }
  it should "compile arith op -" in {
    compilesExp(sdql"1 - 2")
  }
  it should "compile arith op ^" in {
    compilesExp(sdql"3 ^ 2")
  }
  it should "compile arith op /" in {
    compilesExp(sdql"42 / 21")
  }

  it should "compile logical op &&" in {
    compilesExp(sdql"true && true")
    compilesExp(sdql"true && false")
    compilesExp(sdql"false && true")
    compilesExp(sdql"false && false")
  }
  it should "compile logical op ||" in {
    compilesExp(sdql"true || true")
    compilesExp(sdql"true || false")
    compilesExp(sdql"false || true")
    compilesExp(sdql"false || false")
  }
  it should "compile logical ops" in {
    compilesExp(sdql"(3 < 2) && (3 < 4)")
    //    FIXME
    //    compilesExp(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.b < x.c))")
    //    compilesExp(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.c < x.b))")
  }

  it should "compile comparisons on bool" in {
    compilesExp(sdql"true == true")
    compilesExp(sdql"true == false")
    compilesExp(sdql"false != true")
    compilesExp(sdql"false == false")
  }
//  FIXME
//  it should "compile comparisons on map" in {
//    compilesExp(sdql"{} == {}")
//    compilesExp(sdql"{ 1 -> 2 } == {}")
//    compilesExp(sdql"{ 1 -> 2 } != {}")
//    compilesExp(sdql"{ 1 -> 0 } == {}")
//    compilesExp(sdql"{ 1 -> 2 }(1) == 2")
//    compilesExp(sdql"{ 1 -> 2 }(2) == 0")
//    compilesExp(sdql"{ 0 -> { 1 -> 2 } }(0) == { 1 -> 2 }")
//    compilesExp(sdql"{ 0 -> { 1 -> 2 } }(1) == { }")
//  }
//  FIXME
//  it should "compile comparisons" in {
//    compilesExp(sdql"""let R = {<name="Apple"> -> { <name="Apple",initial="A"> -> 1 } } in
//      R(<name="Elephant">) == {}""")
//  }

  it should "compile records" in {
    compilesExp(sdql"< a=1, b=1.5 >")
    compilesExp(sdql"concat(< a=1 >, < b=1.5 >)")
    compilesExp(sdql"concat(< a=1, b=1.5 >, < b=1.5 >)")
    assertThrows[Exception] {
      compilesExp(sdql"concat(< a=1, b=2.5 >, < b=1.5 >)")
    }
  }

//  TODO in future
//  private val iList = 0 until 10
//  private val sList = 100 until 110
//  private val sRel = for(i <- iList; s <- sList) yield (i, s, i * s + 42)
//  private val rRel = for(s <- sList) yield (s, s - 42)
//  private val s = {
//    sRel.map(e => RecordValue(Seq("i" -> e._1, "s" -> e._2, "u" -> e._3)) -> 1).toMap
//  }
//  private val r = {
//    rRel.map(e => RecordValue(Seq("s" -> e._1, "c" -> e._2)) -> 1).toMap
//  }
//
//  it should "compile sums" in {
//    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s_v ")
//    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s ")
//    compilesExp(sdql"let S = $s in sum(<s, s_v> <- S) s_v ")
//    compilesExp(sdql"let S = { } in sum(<s, s_v> <- S) s ")
//    compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
//      let S1 = sum(<s, s_v> <- S) if(s == 1) then {s -> s_v*3} else {}
//      sum(<s, s_v> <- S1) s_v
//      """)
//    compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
//      let S1 = sum(<s, s_v> <- S) if(s == 3) then {s -> s_v*3} else {}
//      sum(<s, s_v> <- S1) s_v
//      """)
//    compilesExp(sdql"""let S = { 1 -> 1.5, 2 -> -1.5 }
//      sum(<s, s_v> <- S) {1 -> s_v}
//      """)
//  }
//
//  it should "compile joins" in {
//    compilesExp(sdql"""
//let S = $s in
//let R = $r in
//let H_R = sum(<x_r, x_r_v> <- R)
//  { < s = x_r.s > -> { < c = x_r.c > -> R(x_r) } }
//sum(<x_s, x_s_v> <- S)
//  let R_p = H_R(<s = x_s.s>)
//  sum(<x_r, x_r_v> <- R_p)
//    { < i = x_s.i, s = x_s.s, c = x_r.c > -> R_p(x_r) * S(x_s) }
//""")
//    compilesExp(sdql"""
//let S = $s in
//let R = $r in
//sum(<x_s, x_s_v> <- S)
//  sum(<x_r, x_r_v> <- R)
//    if (x_r.s == x_s.s) then
//      { < i = x_s.i, s = x_s.s, c = x_r.c > -> R(x_r) * S(x_s) }
//    else
//      { }
//""")
//  }

//  FIXME
//  it should "compile dictionaries" in {
//    compilesExp(sdql"{ 1 -> 1.0 } + { 1 -> 2.0 }")
//    compilesExp(sdql"{ 1 -> 1.0, 2 -> 2.0 } + { 1 -> 2.0, 3 -> 4.0 }")
//    compilesExp(sdql"{ 1 -> 2.0 } * 2.5")
//    compilesExp(sdql"2 * { 1 -> 2.0 }")
//    compilesExp(sdql"{ 1 -> 2.0 } * { 2 -> 2.5 }")
//    compilesExp(sdql"let Q = { <i=1, s=1, c=1, p=1> -> 2 } in Q")
//    compilesExp(sdql"{ 1 -> 5.0 }(1)")
//    compilesExp(sdql"{ 1 -> 5.0 }(2)")
//    compilesExp(sdql"{ 1 -> 0.0 }")
//    compilesExp(sdql"{ 1 -> 1.0 } + { 1 -> -1.0 }")
//    compilesExp(sdql"{ 1 -> promote[enum[double]](1.0) } + { }")
//    compilesExp(sdql"{ 1 -> promote[enum[double]](0.0) } + { }")
//    compilesExp(sdql"{ 1 -> promote[nullable[double]](1.0) } + { 1 -> promote[nullable[double]](-1.0) }")
//    compilesExp(sdql"{ 1 -> promote[nullable[double]](1.0) } + { }")
//    compilesExp(sdql"{ 1 -> promote[nullable[double]](0.0) } + { }")
//  }

//  TODO in future
//  it should "compile semirings" in {
//    compilesExp(sdql"promote[mxsm](1.5)")
//    compilesExp(sdql"promote[mnpr](2)")
//    compilesExp(sdql"promote[max_prod](-1)")
//    compilesExp(sdql"promote[mnsm](2.7) + promote[mnsm](3.2)")
//    compilesExp(sdql"promote[mxsm](2.7) + promote[mxsm](3.2)")
//    compilesExp(sdql"promote[mxsm](2.7) * promote[mxsm](3.2)")
//    compilesExp(sdql"promote[mnsm](2.7) * promote[mnsm](3.2)")
//    compilesExp(sdql"promote[mnpr](2.7) * promote[mnpr](3.2)")
//    compilesExp(sdql"promote[mxpr](2.7) * promote[mxpr](3.2)")
//    compilesExp(sdql"promote[enum[double]](2.7) + promote[enum[double]](3.2)")
//    compilesExp(sdql"promote[enum[double]](2.7) * promote[enum[double]](3.2)")
//    compilesExp(sdql"promote[enum[double]](2.7) + promote[enum[double]](2.7)")
//    compilesExp(sdql"promote[enum[double]](2.7) * promote[enum[double]](2.7)")
//    compilesExp(sdql"promote[nullable[double]](2.7) + promote[nullable[double]](3.2)")
//    compilesExp(sdql"promote[nullable[double]](2.7) * promote[nullable[double]](3.2)")
//    compilesExp(sdql"promote[nullable[double]](2.7) + promote[nullable[double]](2.7)")
//    compilesExp(sdql"promote[nullable[double]](2.7) * promote[nullable[double]](2.7)")
//    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) promote[mxsm](s_v) ")
//    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) promote[mxpr](s_v) ")
//    compilesExp(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) promote[mnsm](s_v) ")
//  }

//  FIXME
//  it should "compile external functions" in {
//    compilesExp(sdql"""ext(`ParseDate`, "1989-07-13")""")
//    compilesExp(sdql"""ext(`Year`, ext(`ParseDate`, "1989-07-13"))""")
//    compilesExp(sdql"""ext(`SubString`, "19890713", 1, 2)""")
//    compilesExp(sdql"""ext(`StrStartsWith`, "19890713", "1989")""")
//    compilesExp(sdql"""ext(`StrStartsWith`, "19890713", "199")""")
//    compilesExp(sdql"""ext(`StrEndsWith`, "19890713", "0713")""")
//    compilesExp(sdql"""ext(`StrEndsWith`, "19890713", "113")""")
//    compilesExp(sdql"""ext(`StrContains`, "19890713", "8907")""")
//    compilesExp(sdql"""ext(`StrContains`, "19890713", "1989")""")
//    compilesExp(sdql"""ext(`StrContains`, "19890713", "0713")""")
//    compilesExp(sdql"""ext(`StrContains`, "19890713", "901")""")
//    compilesExp(sdql"""ext(`StrContains`, "19890713", "113")""")
//    compilesExp(sdql"""ext(`StrContainsN`, "19890713", "1989", "07")""")
//    compilesExp(sdql"""ext(`StrContainsN`, "19890713", "8907", "1989", "0713")""")
//    compilesExp(sdql"""ext(`StrContainsN`, "19890713", "19892", "0713")""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "8907", 0)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "8907", 2)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "8907", 3)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "1989", 0)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "1989", 1)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "0713", 0)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "901", 0)""")
//    compilesExp(sdql"""ext(`StrIndexOf`, "19890713", "113", 0)""")
//  }

//  TODO
//  it should "compile simple graph queries" in {
//    compilesExp(sdql"""let Nodes = {
//      0 -> <label = {"Person" -> true, "Director" -> true, "Singer" -> true}, name="Oliver Stone", age=30>,
//      1 -> <label = {"Person" -> true, "Director" -> true}, name="Michael Douglas", age=35>,
//      2 -> <label = {"Person" -> true, "Actor" -> true}, name="Charlie Sheen",age=32>}
//    sum(<k,v> in Nodes)
//      if(v.name=="Charlie Sheen") then
//        {<age=v.age> -> 1}
//      else
//        {}
//    """)
//  }

  // TODO all these test cases above were taken from the interpreter tests
  //  separate test execution from test data and have them both share the data

  it should "compile and run TPCH Q1" in {
    compileAndRunFile("progs/tpch/q1.sdql")
  }
  it should "compile and run TPCH Q2" in {
    compileAndRunFile("progs/tpch/q2.sdql")
  }
  it should "compile and run TPCH Q3" in {
    compileAndRunFile("progs/tpch/q3.sdql")
  }
  it should "compile and run TPCH Q4" in {
    compileAndRunFile("progs/tpch/q4.sdql")
  }
  it should "compile and run TPCH Q5" in {
    compileAndRunFile("progs/tpch/q5.sdql")
  }
  it should "compile and run TPCH Q6" in {
    compileAndRunFile("progs/tpch/q6.sdql")
  }
  it should "compile and run TPCH Q7" in {
    compileAndRunFile("progs/tpch/q7.sdql")
  }
// FIXME
//  it should "compile and run TPCH Q8" in {
//    compileAndRunFile("progs/tpch/q8.sdql")
//  }
  it should "compile and run TPCH Q9" in {
    compileAndRunFile("progs/tpch/q9.sdql")
  }
  it should "compile and run TPCH Q10" in {
    compileAndRunFile("progs/tpch/q10.sdql")
  }
// FIXME
//  it should "compile and run TPCH Q11" in {
//    compileAndRunFile("progs/tpch/q11.sdql")
//  }
// FIXME
//  it should "compile and run TPCH Q12" in {
//    compileAndRunFile("progs/tpch/q12.sdql")
//  }
  it should "compile and run TPCH Q13" in {
    compileAndRunFile("progs/tpch/q13.sdql")
  }
  it should "compile and run TPCH Q14" in {
    compileAndRunFile("progs/tpch/q14.sdql")
  }
// FIXME
//  it should "compile and run TPCH Q15" in {
//    compileAndRunFile("progs/tpch/q15.sdql")
//  }
  it should "compile and run TPCH Q16" in {
    compileAndRunFile("progs/tpch/q16.sdql")
  }
  it should "compile and run TPCH Q17" in {
    compileAndRunFile("progs/tpch/q17.sdql")
  }
// FIXME
//  it should "compile and run TPCH Q18" in {
//    compileAndRunFile("progs/tpch/q18.sdql")
//  }
  it should "compile and run TPCH Q19" in {
    compileAndRunFile("progs/tpch/q19.sdql")
  }
  it should "compile and run TPCH Q20" in {
    compileAndRunFile("progs/tpch/q20.sdql")
  }
  it should "compile and run TPCH Q21" in {
    compileAndRunFile("progs/tpch/q21.sdql")
  }
  it should "compile and run TPCH Q22" in {
    compileAndRunFile("progs/tpch/q22.sdql")
  }

//  TODO rid of compilesFile - but since we're using compileAndRunFile create test files in datasets/tpch/tests/*.sdql
//  private def compilesFile(path: String) = compilesExp(SourceCode.fromFile(path).exp)
  private def compileAndRunFile(path: String) =
    CppCompilation.compile(java.nio.file.Path.of(path), CppCodegen(SourceCode.fromFile(path).exp))
  private def compilesExp(e: Exp) = assert(compile(e) == 0)
  private def compile(exp: Exp) = fromCpp(CppCodegen(exp))
  private def fromCpp(cpp: String) = CppCompilation.inGeneratedDir(Seq("bash", "-c", cmd(escape(cpp)))).run().exitValue()
  private def cmd(cpp: String) = s"${CppCompilation.clangCmd.mkString(" ")} -xc++ -fsyntax-only - <<< '$cpp'"
  // TODO avoid escaping single quotes in source code
  private def escape(cpp: String) = cpp.replace(singleQuote.toString, s"char(${singleQuote.toInt})")
  private val singleQuote = '\''
}
