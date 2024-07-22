package sdql
package backend

import ir._
import frontend._
import org.scalatest._
import Matchers._

class InterpreterTest extends FlatSpec {

  def interpreter(e: Exp) = Interpreter(e)

  "Interpreter" should "work for constants" in {
    interpreter(sdql"true") should === (true)
    interpreter(sdql"false") should === (false)
    interpreter(sdql"42") should be (42)
    interpreter(sdql"42.2") should be (42.2)
    interpreter(sdql""" "foo" """) should be ("foo")
    interpreter(sdql"date(19700101)") should be (DateValue(19700101))
    interpreter(sdql"< a = 1, b = 2 >") should be (RecordValue(Seq("a" -> 1, "b" -> 2)))
    interpreter(sdql"""{ "a" -> 1, "b" -> 2 }""") should be (Map("a" -> 1, "b" -> 2))
    interpreter(sdql"""{ "a" -> 1, "b" -> 2.5 }""") should be (Map("a" -> 1.0, "b" -> 2.5))
  }

  it should "work for arith ops" in {
    interpreter(sdql"1 * 2") should be (2)
    interpreter(sdql"1 + 2") should be (3)
    interpreter(sdql"1 - 2") should be (-1)
    interpreter(sdql"3 ^ 2") should be (9)
    interpreter(sdql"42 / 21") should be (2)
  }

  it should "work for logical ops" in {
    interpreter(sdql"true && true") should === (true)
    interpreter(sdql"true && false") should === (false)
    interpreter(sdql"false && true") should === (false)
    interpreter(sdql"false && false") should === (false)
    interpreter(sdql"true || true") should === (true)
    interpreter(sdql"true || false") should === (true)
    interpreter(sdql"false || true") should === (true)
    interpreter(sdql"false || false") should === (false)
    interpreter(sdql"(3 < 2) && (3 < 4)") should === (false)
    interpreter(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.b < x.c))") should === (true)
    interpreter(sdql"let x=<a=1,b=2,c=3> in ((x.a < x.b) && (x.c < x.b))") should === (false)
  }

  it should "work for comparison" in {
    interpreter(sdql"true == true") should === (true)
    interpreter(sdql"true == false") should === (false)
    interpreter(sdql"false != true") should === (true)
    interpreter(sdql"false == false") should === (true)
    interpreter(sdql"{} == {}") should === (true)
    interpreter(sdql"{ 1 -> 2 } == {}") should === (false)
    interpreter(sdql"{ 1 -> 2 } != {}") should === (true)
    interpreter(sdql"{ 1 -> 0 } == {}") should === (true)
    interpreter(sdql"{ 1 -> 2 }(1) == 2") should === (true)
    interpreter(sdql"{ 1 -> 2 }(2) == 0") should === (true)
    interpreter(sdql"{ 0 -> { 1 -> 2 } }(0) == { 1 -> 2 }") should === (true)
    interpreter(sdql"{ 0 -> { 1 -> 2 } }(1) == { }") should === (true)
    interpreter(sdql"""let R = {<name="Apple"> -> { <name="Apple",initial="A"> -> 1 } } in
      R(<name="Elephant">) == {}""") should === (true)
  }

  it should "work for records" in {
    interpreter(sdql"< >") should be (RecordValue(Nil))
    interpreter(sdql"< a=1, b=1.5 >") should be (RecordValue(Seq("a" -> 1, "b" -> 1.5)))
    interpreter(sdql"concat(< a=1 >, < b=1.5 >)") should be (RecordValue(Seq("a" -> 1, "b" -> 1.5)))
    interpreter(sdql"concat(< a=1, b=1.5 >, < b=1.5 >)") should be (RecordValue(Seq("a" -> 1, "b" -> 1.5)))
    assertThrows[Exception] {
      interpreter(sdql"concat(< a=1, b=2.5 >, < b=1.5 >)")
    }
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
    interpreter(sdql"let S = { } in sum(<s, s_v> <- S) s ") should be (ZeroValue)
    interpreter(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
      let S1 = sum(<s, s_v> <- S) if(s == 1) then {s -> s_v*3} else {}
      sum(<s, s_v> <- S1) s_v
      """) should be (4.5)
    interpreter(sdql"""let S = { 1 -> 1.5, 2 -> 2.5 }
      let S1 = sum(<s, s_v> <- S) if(s == 3) then {s -> s_v*3} else {}
      sum(<s, s_v> <- S1) s_v
      """) should be (ZeroValue)
    interpreter(sdql"""let S = { 1 -> 1.5, 2 -> -1.5 }
      sum(<s, s_v> <- S) {1 -> s_v}
      """) should be (Map())
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
    interpreter(sdql"{ 1 -> 0.0 }") should be (interpreter(sdql"{ }"))
    interpreter(sdql"{ 1 -> 1.0 } + { 1 -> -1.0 }") should be (interpreter(sdql"{ }"))
    interpreter(sdql"{ 1 -> promote[enum[double]](1.0) } + { }") should be (interpreter(sdql"{ 1 -> promote[enum[double]](1.0) }"))
    interpreter(sdql"{ 1 -> promote[enum[double]](0.0) } + { }") should be (interpreter(sdql"{ 1 -> promote[enum[double]](0.0) }"))
    interpreter(sdql"{ 1 -> promote[nullable[double]](1.0) } + { 1 -> promote[nullable[double]](-1.0) }") should be (interpreter(sdql"{ 1 -> promote[nullable[double]](0.0) }"))
    interpreter(sdql"{ 1 -> promote[nullable[double]](1.0) } + { }") should be (interpreter(sdql"{ 1 -> promote[nullable[double]](1.0) }"))
    interpreter(sdql"{ 1 -> promote[nullable[double]](0.0) } + { }") should be (interpreter(sdql"{ 1 -> promote[nullable[double]](0.0) }"))
  }

  it should "work for semirings" in {
    interpreter(sdql"promote[mxsm](1.5)") should be (MaxSumSemiRing(Some(1.5)))
    interpreter(sdql"promote[mnpr](2)") should be (MinProdSemiRing(Some(2)))
    interpreter(sdql"promote[max_prod](-1)") should be (MaxProdSemiRing(Some(-1)))
    interpreter(sdql"promote[mnsm](2.7) + promote[mnsm](3.2)") should be (MinSumSemiRing(Some(2.7)))
    interpreter(sdql"promote[mxsm](2.7) + promote[mxsm](3.2)") should be (MaxSumSemiRing(Some(3.2)))
    interpreter(sdql"promote[mxsm](2.7) * promote[mxsm](3.2)") should be (MaxSumSemiRing(Some(5.9)))
    interpreter(sdql"promote[mnsm](2.7) * promote[mnsm](3.2)") should be (MinSumSemiRing(Some(5.9)))
    interpreter(sdql"promote[mnpr](2.7) * promote[mnpr](3.2)") should be (MinProdSemiRing(Some(2.7 * 3.2)))
    interpreter(sdql"promote[mxpr](2.7) * promote[mxpr](3.2)") should be (MaxProdSemiRing(Some(2.7 * 3.2)))
    interpreter(sdql"promote[enum[double]](2.7) + promote[enum[double]](3.2)") should be (EnumSemiRing(EnumSemiRingType(RealType), TopEnumSemiRing))
    interpreter(sdql"promote[enum[double]](2.7) * promote[enum[double]](3.2)") should be (EnumSemiRing(EnumSemiRingType(RealType), BottomEnumSemiRing))
    interpreter(sdql"promote[enum[double]](2.7) + promote[enum[double]](2.7)") should be (EnumSemiRing(EnumSemiRingType(RealType), SingletonEnumSemiRing(2.7)))
    interpreter(sdql"promote[enum[double]](2.7) * promote[enum[double]](2.7)") should be (EnumSemiRing(EnumSemiRingType(RealType), SingletonEnumSemiRing(2.7)))
    interpreter(sdql"promote[nullable[double]](2.7) + promote[nullable[double]](3.2)") should be (NullableSemiRing(NullableSemiRingType(RealType), Some(5.9)))
    interpreter(sdql"promote[nullable[double]](2.7) * promote[nullable[double]](3.2)") should be (NullableSemiRing(NullableSemiRingType(RealType), Some(2.7 * 3.2)))
    interpreter(sdql"promote[nullable[double]](2.7) + promote[nullable[double]](2.7)") should be (NullableSemiRing(NullableSemiRingType(RealType), Some(5.4)))
    interpreter(sdql"promote[nullable[double]](2.7) * promote[nullable[double]](2.7)") should be (NullableSemiRing(NullableSemiRingType(RealType), Some(2.7 * 2.7)))
    interpreter(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) promote[mxsm](s_v) ") should be (MaxSumSemiRing(Some(2.5)))
    interpreter(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) promote[mxpr](s_v) ") should be (MaxProdSemiRing(Some(2.5)))
    interpreter(sdql"let S = { 1 -> 1.5, 2 -> 2.5 } in sum(<s, s_v> <- S) promote[mnsm](s_v) ") should be (MinSumSemiRing(Some(1.5)))
  }

  it should "handle external functions" in {
    interpreter(sdql"""ext(`ParseDate`, "1989-07-13")""") should be (DateValue(19890713))
    interpreter(sdql"""ext(`Year`, ext(`ParseDate`, "1989-07-13"))""") should be (1989)
    interpreter(sdql"""ext(`SubString`, "19890713", 1, 2)""") should be ("98")
    interpreter(sdql"""ext(`StrStartsWith`, "19890713", "1989")""") should === (true)
    interpreter(sdql"""ext(`StrStartsWith`, "19890713", "199")""") should === (false)
    interpreter(sdql"""ext(`StrEndsWith`, "19890713", "0713")""") should === (true)
    interpreter(sdql"""ext(`StrEndsWith`, "19890713", "113")""") should === (false)
    interpreter(sdql"""ext(`StrContains`, "19890713", "8907")""") should === (true)
    interpreter(sdql"""ext(`StrContains`, "19890713", "1989")""") should === (true)
    interpreter(sdql"""ext(`StrContains`, "19890713", "0713")""") should === (true)
    interpreter(sdql"""ext(`StrContains`, "19890713", "901")""") should === (false)
    interpreter(sdql"""ext(`StrContains`, "19890713", "113")""") should === (false)
    interpreter(sdql"""ext(`StrContainsN`, "19890713", "1989", "07")""") should === (true)    
    interpreter(sdql"""ext(`StrContainsN`, "19890713", "8907", "1989", "0713")""") should === (true)    
    interpreter(sdql"""ext(`StrContainsN`, "19890713", "19892", "0713")""") should === (false)    
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "8907", 0)""") should === (2)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "8907", 2)""") should === (2)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "8907", 3)""") should === (-1)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "1989", 0)""") should === (0)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "1989", 1)""") should === (-1)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "0713", 0)""") should === (4)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "901", 0)""") should === (-1)
    interpreter(sdql"""ext(`StrIndexOf`, "19890713", "113", 0)""") should === (-1)
  }

  it should "handle simple graph queries" in {
    val q1 = sdql"""let Nodes = { 
      0 -> <label = {"Person" -> true, "Director" -> true, "Singer" -> true}, name="Oliver Stone", age=30>, 
      1 -> <label = {"Person" -> true, "Director" -> true}, name="Michael Douglas", age=35>,
      2 -> <label = {"Person" -> true, "Actor" -> true}, name="Charlie Sheen",age=32>}  
    sum(<k,v> in Nodes) 
      if(v.name=="Charlie Sheen") then
        {<age=v.age> -> 1}
      else
        {}
    """
    interpreter(q1) should be (interpreter(sdql"{<age=32> -> 1}"))
  }

  it should "load correctly" in {
    val file = "test.csv"
    def writeToFile(str: String): Unit = {
      val pw = new java.io.PrintWriter(file)
      pw.println(str)
      pw.close
    }
    writeToFile("1|one|2.5|1989-07-13")
    interpreter(sdql"""load[{<a:int, b: string, c: real, d: date> -> int}]($file)""") should be (interpreter(sdql"""{ <a=1, b="one", c=2.5, d=ext(`ParseDate`, "1989-07-13")> -> 1 }"""))
    writeToFile("1|1|2.5|1989-07-13")
    interpreter(sdql"""load[{<a:int, b: int, c: real, d: date> -> int}]($file)""") should be (interpreter(sdql"""{ <a=1, b=1, c=2.5, d=ext(`ParseDate`, "1989-07-13")> -> 1 }"""))
    writeToFile("1|1|0.08|1989-07-13")
    interpreter(sdql"""let R = load[{<a:int, b: int, c: real, d: date> -> int}]($file)
      sum(<x, v> <- R) if(x.c <= 0.08) then v else 0""") should be (1)
    writeToFile("""933|4139
933|6597069777240
933|10995116284808
933|32985348833579
933|32985348838375
1129|1242
1129|2199023262543
1129|6597069771886
1129|6597069776731
1129|8796093026744""")
    interpreter(sdql"""let R = load[{<startId:int,endId:int>->int}]($file)
      sum(<x, v> <- R) if(x.startId == 933) then 10 else 1""") should be (55)
  }
}
