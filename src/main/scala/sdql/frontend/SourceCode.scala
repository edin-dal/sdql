package sdql
package frontend

import ir._

class SourceCode(val fileName: String, val exp: Exp)

object SourceCode {
  def fromFile(fileName: String): SourceCode = {
    val content = scala.io.Source.fromFile(fileName).mkString
    new SourceCode(fileName, Parser(content))
  }
}
