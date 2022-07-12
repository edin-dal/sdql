package sdql
package backend

import ir._
import frontend._
import org.scalatest._
import Matchers._

class InterpreterTest extends FlatSpec {

  def interpreter(e: Exp) = Interpreter(e)

  "Interpreter" should "work for constants" in {
    interpreter(sdql"true") === true
    interpreter(sdql"false") === false
    interpreter(sdql"42") should be (42)
    interpreter(sdql"42.2") should be (42.2)
    interpreter(sdql""" "foo" """) should be ("foo")
    interpreter(sdql"< a = 1, b = 2 >") should be (RecordValue(Seq("a" -> 1, "b" -> 2)))
    interpreter(sdql"""{ "a" -> 1, "b" -> 2 }""") should be (Map("a" -> 1, "b" -> 2))
  }

  it should "work for arith ops" in {
    interpreter(sdql"1 * 2") should be (2)
    interpreter(sdql"1 + 2") should be (3)
    interpreter(sdql"1 - 2") should be (-1)
    interpreter(sdql"3 ^ 2") should be (9)
    interpreter(sdql"42 / 21") should be (2)
  }

  it should "work for logical ops" in {
    interpreter(sdql"true && true") === true
    interpreter(sdql"true && false") === false
    interpreter(sdql"false && true") === false
    interpreter(sdql"false && false") === false
    interpreter(sdql"true || true") === true
    interpreter(sdql"true || false") === true
    interpreter(sdql"false || true") === true
    interpreter(sdql"false || false") === false
    interpreter(sdql"(3 < 2) && (3 < 4)") === false
    interpreter(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.b < x.c))") === true
    interpreter(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.c < x.b))") === false
  }

  val iList = 0 until 10
  val sList = 100 until 110
  val sRel = for(i <- iList; s <- sList) yield (i, s, i * s + 42)
  val rRel = for(s <- sList) yield (s, s - 42)
  val s_join_rRel = sRel.map(e => (e._1, e._2, rRel.toMap.apply(e._2)))

  val s = {
    sRel.map(e => RecordValue(Seq("i" -> e._1, "s" -> e._2, "u" -> e._3)) -> 1).toMap
  }
  val r = {
    rRel.map(e => RecordValue(Seq("s" -> e._1, "c" -> e._2)) -> 1).toMap
  }
  val s_join_r = {
    interpreter(DictNode(s_join_rRel.map(e => (sdql"< i = ${e._1}, s = ${e._2}, c = ${e._3} >", Const(1)))))
  }

  it should "work for sum" in {
    interpreter(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s_v ") should be (4.0)
    interpreter(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) s ") should be (3)
    interpreter(sdql"let S = $s in sum(<s, s_v> <- S) s_v ") should be (iList.size * sList.size)
  }

  it should "work for joins" in {
    interpreter(sdql"""
let S = $s in 
let R = $r in 
let H_R = sum(<x_r, x_r_v> <- R)
  { < s = x_r.s > -> { < c = x_r.c > -> R(x_r) } }
sum(<x_s, x_s_v> <- S) 
  let R_p = H_R(<s = x_s.s>)
  sum(<x_r, x_r_v> <- R_p)
    { < i = x_s.i, s = x_s.s, c = x_r.c > -> R_p(x_r) * S(x_s) }
""") should be (s_join_r)
    interpreter(sdql"""
let S = $s in 
let R = $r in 
sum(<x_s, x_s_v> <- S) 
  sum(<x_r, x_r_v> <- R)
    if (x_r.s == x_s.s) then
      { < i = x_s.i, s = x_s.s, c = x_r.c > -> R(x_r) * S(x_s) }
    else 
      { }
""") should be (s_join_r)
  }

  it should "work for dictionaries" in {
    interpreter(sdql"{ 1 -> 1.0 } + { 1 -> 2.0 }") should be (interpreter(sdql"{ 1 -> 3.0 }"))
    interpreter(sdql"{ 1 -> 1.0, 2 -> 2.0 } + { 1 -> 2.0, 3 -> 4.0 }") should be (interpreter(sdql"{ 1 -> 3.0, 2 -> 2.0, 3 -> 4.0 }"))
    interpreter(sdql"{ 1 -> 2.0 } * 2.5") should be (interpreter(sdql"{ 1 -> 5.0 }"))
    interpreter(sdql"2 * { 1 -> 2.0 }") should be (interpreter(sdql"{ 1 -> 4.0 }"))
    interpreter(sdql"{ 1 -> 2.0 } * { 2 -> 2.5 }") should be (interpreter(sdql"{ 1 -> { 2 -> 5.0 } }"))
    interpreter(sdql"let Q = { <i=1, s=1, c=1, p=1> -> 2 } in Q") should be (interpreter(sdql"{ <i=1, s=1, c=1, p=1> -> 2 }"))
    interpreter(sdql"{ 1 -> 5.0 }(1)") should be (5.0)
    interpreter(sdql"{ 1 -> 5.0 }(2)") should be (ZeroValue)
  }

  it should "handle external functions" in {
    interpreter(sdql"""ext(`ParseDate`, "1989-07-13")""") should be (DateValue(19890713))
    interpreter(sdql"""ext(`Year`, ext(`ParseDate`, "1989-07-13"))""") should be (1989)
    interpreter(sdql"""ext(`SubString`, "19890713", 1, 2)""") should be ("98")
    interpreter(sdql"""ext(`StrStartsWith`, "19890713", "1989")""") === true
    interpreter(sdql"""ext(`StrStartsWith`, "19890713", "199")""") === false
    interpreter(sdql"""ext(`StrEndsWith`, "19890713", "0713")""") === true
    interpreter(sdql"""ext(`StrEndsWith`, "19890713", "113")""") === false
    interpreter(sdql"""ext(`StrContains`, "19890713", "8907")""") === true
    interpreter(sdql"""ext(`StrContains`, "19890713", "1989")""") === true
    interpreter(sdql"""ext(`StrContains`, "19890713", "0713")""") === true
    interpreter(sdql"""ext(`StrContains`, "19890713", "901")""") === false
    interpreter(sdql"""ext(`StrContains`, "19890713", "113")""") === false
    interpreter(sdql"""ext(`StrContainsN`, "19890713", "1989", "07")""") === true    
    interpreter(sdql"""ext(`StrContainsN`, "19890713", "8907", "1989", "0713")""") === true    
    interpreter(sdql"""ext(`StrContainsN`, "19890713", "19892", "0713")""") === false    
  }

  it should "load correctly" in {
    val file = "test.csv"
    val pw = new java.io.PrintWriter(file)
    pw.println("1|one|2.5|1989-07-13")
    pw.close
    interpreter(sdql"""load[{<a:int, b: string, c: real, d: date> -> int}]($file)""") should be (interpreter(sdql"""{ <a=1, b="one", c=2.5, d=ext(`ParseDate`, "1989-07-13")> -> 1 }"""))
  }
}
