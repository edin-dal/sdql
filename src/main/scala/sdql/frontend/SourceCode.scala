package sdql
package frontend

import ir._

class SourceCode(val fileName: String, val exp: Exp)

object SourceCode {
  def fromFile(fileName: String): SourceCode = {
    val source = scala.io.Source.fromFile(fileName)
    val content = try source.mkString finally source.close()
    new SourceCode(fileName, Parser(content))
  }
}
