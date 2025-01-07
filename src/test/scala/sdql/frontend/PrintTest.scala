package sdql
package frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sdql.ir.Print

import java.nio.file.{ Files, Paths }
import scala.jdk.CollectionConverters.*

class PrintTest extends AnyFlatSpec with Matchers {
  private val progsDir = Paths.get(System.getProperty("user.dir")).resolve("progs")

  it should "check formatting of SDQL programs" in Files
    .walk(progsDir)
    .iterator()
    .asScala
    .filter(Files.isRegularFile(_))
    .filter(_.toString.endsWith(".sdql"))
    .foreach(path => checkFormat(path.toString))

  private def checkFormat(fileName: String) = {
    val src = SourceCode.fromFile(fileName)
    src.content shouldEqual Print.pretty(src.exp)
  }
}
