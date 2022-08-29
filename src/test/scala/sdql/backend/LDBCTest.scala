package sdql.backend

import org.scalatest.FlatSpec
import sdql.ir.Exp
import org.scalatest._
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import sdql.frontend.Interpolator
import sdql.{frontend, ir}

class LDBCTest extends FlatSpec{
  def interpreter(e: Exp) = Interpreter(e)
//  it should "load correctly" in {
//    val file = "test.csv"
//    def writeToFile(str: String): Unit = {
//      val pw = new java.io.PrintWriter(file)
//      pw.println(str)
//      pw.close
//    }
//    writeToFile("1|one|2.5|")
//    interpreter(sdql"""load[{<a:int, b: string, c: real> -> int}]($file)""") should be (interpreter(sdql"""{ <a=1, b="one", c=2.5> -> 1 }"""))
//    writeToFile("1|1|2.5|1989-07-13|")
//    interpreter(sdql"""load[{<a:int, b: int, c: real, d: date> -> int}]($file)""") should be (interpreter(sdql"""{ <a=1, b=1, c=2.5, d=ext(`ParseDate`, "1989-07-13")> -> 1 }"""))
//    writeToFile("1|1|0.08|1989-07-13|")
//    interpreter(sdql"""let R = load[{<a:int, b: int, c: real, d: date> -> int}]($file)
//      sum(<x, v> <- R) if(x.c <= 0.08) then v else 0""") should be (1)
//  }
//
//  it should "handle find one node" in {
//    val q1 = sdql"""
//      let Person = {
//      0 -> <name="Oliver Stone",gender="female",age=30>,
//      1 -> <name="Michael Douglas",gender="male",age=35>,
//      2 -> <name="Charlie Sheen",gender="female",age=40>,
//      3 -> <name="kenny wang",gender="male",age=40>}
//      sum(<k,p> in Person)
//      if(p.age==35) then
//      {<name=p.name>->1}
//      else{}
//      """
//    interpreter(q1) should be (interpreter(sdql"""{<name="Michael Douglas"> -> 1}"""))
//  }
//  it should "handle find one relationship with relationship property" in { // MATCH (p1:Person)-[:Knows{creationDate:'1125'}]->(p2:Person) return p1.name
//    val q1 = sdql"""let Knows = {1->
//      {<startId=0,endId=1,creationDate="1123">->1,
//      <startId=1,endId=2,creationDate="1124">->1,
//      <startId=2,endId=3,creationDate="1125">->1,
//      <startId=1,endId=0,creationDate="1123">->1,
//      <startId=2,endId=1,creationDate="1124">->1,
//      <startId=3,endId=2,creationDate="1125">->1}}
//      let Person = {
//      0 -> <name="Oliver Stone",gender="female",age=30>,
//      1 -> <name="Michael Douglas",gender="male",age=35>,
//      2 -> <name="Charlie Sheen",gender="female",age=32>,
//      3 -> <name="kenny wang",gender="male",age=40>}
//      let RelLabels={"Knows"-> {<startLabel="Person",endLabel="Person">->1}}
//
//      let RelId=sum(<i,j> in RelLabels("Knows"))
//      if((i.startLabel=="Person")&& i.endLabel=="Person") then
//      {1-><id=j>}
//      else{}
//      sum(<r,v> in Knows(RelId(1).id))
//      if(r.creationDate=="1125") then
//      let p2=Person(r.endId) in
//      let p1=Person(r.startId) in
//      {<name=p1.name>->1}
//      else{}
//      """
//    interpreter(q1) should be (interpreter(sdql"""{<name="Charlie Sheen">->1,<name="kenny wang">->1}"""))
//  }
//  it should "handle find one relationship with node property" in { // MATCH (p1:Person {age:32})-[:Knows{creationDate:'1125'}]->(p2:Person) return p2.name result:{<name="Charlie Sheen">->1}
//    val q1 = sdql"""let Knows = {1->
//      {<startId=0,endId=1,creationDate="1123">->1,
//      <startId=1,endId=2,creationDate="1124">->1,
//      <startId=2,endId=3,creationDate="1125">->1,
//      <startId=1,endId=0,creationDate="1123">->1,
//      <startId=2,endId=1,creationDate="1124">->1,
//      <startId=3,endId=2,creationDate="1125">->1}}
//      let Person = {
//      0 -> <name="Oliver Stone",gender="female",age=30>,
//      1 -> <name="Michael Douglas",gender="male",age=35>,
//      2 -> <name="Charlie Sheen",gender="female",age=32>,
//      3 -> <name="kenny wang",gender="male",age=32>}
//      let RelLabels={"Knows"-> {<startLabel="Person",endLabel="Person">->1}}
//
//      let RelId=sum(<i,j> in RelLabels("Knows"))
//      if((i.startLabel=="Person")&& i.endLabel=="Person") then
//      {1-><id=j>}
//      else{}
//      sum(<r,v> in Knows(RelId(1).id))
//      if((r.creationDate=="1125") && Person(r.startId).age==32) then
//      let p2=Person(r.endId) in
//        let p1=Person(r.startId) in
//        {<name=p2.name>->1}
//          else{}
//          """
//    interpreter(q1) should be (interpreter(sdql"""{<name="Charlie Sheen">->1,<name="kenny wang">->1}"""))
//  }
//  it should "handle find two relationships" in { // MATCH (p1:Person {age:32})-[:Knows{creationDate:'1125'}]->(p2:Person)-[:studyAt{classYear:1997}]->(p3:University) return p3.url result:{<url="http4">->1}
//    val q1 = sdql"""let Knows = {1->
//      {<startId=0,endId=1,creationDate="1123">->1,
//      <startId=1,endId=2,creationDate="1124">->1,
//      <startId=2,endId=3,creationDate="1125">->1,
//      <startId=1,endId=0,creationDate="1123">->1,
//      <startId=2,endId=1,creationDate="1124">->1,
//      <startId=3,endId=2,creationDate="1125">->1}}
//      let studyAt={1->
//      {<startId=0,endId=1,classYear="2011">->1,
//      <startId=1,endId=2,classYear="2012">->1,
//      <startId=2,endId=3,classYear="1998">->1,
//      <startId=3,endId=3,classYear="1997">->1}}
//
//      let Person = {
//      0 -> <name="Oliver Stone",gender="female",age=30>,
//      1 -> <name="Michael Douglas",gender="male",age=35>,
//      2 -> <name="Charlie Sheen",gender="female",age=32>,
//      3 -> <name="kenny wang",gender="male",age=40>}
//
//      let University={
//      0 -> <name="Ed University",url="http1">,
//      1 -> <name="B University",url="http2">,
//      2 -> <name="C University",url="http3">,
//      3 -> <name="D University",url="http4">}
//      let RelLabels={"Knows"->{<startLabel="Person",endLabel="Person">->1},"studyAt"->{<startLabel="Person",endLabel="University">->1}}
//
//      let RelId=sum(<i,j> in RelLabels("Knows"))
//      if((i.startLabel=="Person")&& i.endLabel=="Person") then
//      {1-><id=j>}
//      else{}
//      let R1=sum(<r,v> in Knows(RelId(1).id))
//      if((r.creationDate=="1125") && Person(r.startId).age==32) then
//      {r->1}
//      else{}
//      let RelId2=sum(<i,j> in RelLabels("studyAt"))
//      if((i.startLabel=="Person")&& i.endLabel=="University") then
//      {1-><id=j>}
//      else{}
//      let R2=sum(<r,v> in studyAt(RelId2(1).id))
//      if(r.classYear=="1997") then
//      {r->1}
//      else{}
//      sum(<r1,v1> in R1)sum(<r2,v2> in R2)
//      if(r1.endId==r2.startId) then
//      let p1=Person(r1.startId)
//      let p2=Person(r1.endId)
//      let p3=University(r2.endId)
//      {<url=p3.url>->1}
//      else{}
//      """
//    interpreter(q1) should be (interpreter(sdql"""{<url="http4">->1}"""))
//  }

//  it should "load nodes correctly" in {
//    interpreter(sdql"""load[{long-> <id:long,firstName:string,lastName:string,gender:string,birthday:date,creationDate:string,locationIP:string,browserUsed:string>}]("Person.csv")""") should be (1)
//  }

//  it should "load rel correctly" in {
//    val q=
//      sdql"""let knows={1->load[{<startId:int,endId:int,creationDate:string>->int}]("knows.csv"),2->load[{<startId:int,endId:int,creationDate:string>->int}]("knows.csv")}
//        sum(<k,v> in knows)
//        {<id=k>->1}
//      """
//    interpreter(q) should be (interpreter(sdql"{<id=1> -> 1}"))
//  }

  it should "s1 correctly" in {
    val q=sdql"""
        let Person=load[{long-> <id:long,firstName:string,lastName:string,gender:string,birthday:date,creationDate:string,locationIP:string,browserUsed:string>}]("LDBCdatasets/Person.csv")
        let City=load[{long-> <id:long,name:string,url:string>}]("LDBCdatasets/City.csv")
        let knows={1->load[{<startId:long,endId:long,creationDate:string>->int}]("LDBCdatasets/knows.csv")}
        let isLocatedIn={1->load[{<startId:int,endId:int>->int}]("LDBCdatasets/person_isLocatedIn_place.csv")}
        let RelLabels={"isLocatedIn"-> {<startLabel="Person",endLabel="City">->1}}
        sum(<k,v> in Person)
        {<id=v>->1}
        """
    interpreter(q) should be (interpreter(sdql"{<id=1> -> 1}"))
  }

//    it should "s1 correctly" in {
//      val q=sdql"""
//          let knows={1->load[{<startId:long,endId:long,creationDate:string>->int}]("LDBCdatasets/knows.csv")}
//          sum(<k,v> in knows(1))
//          {<id=k.creationDate>->1}
//          """
//      interpreter(q) should be (interpreter(sdql"{<id=1> -> 1}"))
//    }
}
