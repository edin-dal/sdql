package sdql
package frontend

import ir._

class SourceCode(val fileName: String, val exp: Exp)

object SourceCode {
  def fromFile(fileName: String, patch: String => String = identity): SourceCode = {
    val source = scala.io.Source.fromFile(fileName)
    val content = try source.mkString finally source.close()
    // patching is for tests which change the datasets directory
    val patched = patch(content)
    new SourceCode(fileName, Parser(patched))
  }
}
