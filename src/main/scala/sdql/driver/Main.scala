package sdql
package driver

import sdql.backend._
import sdql.frontend._
import sdql.ir._

import java.nio.file.Path
import scala.sys.process._

object Main {

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      raise("usage: `run <cmd> <args>*`")
    }
    args(0) match {
      case "interpret" =>
        if (args.length < 3) {
          raise("usage: `run interpret <path> <sdql_files>*`")
        }
        val dirPath = Path.of(args(1))
        for (fileName <- args.drop(2)) {
          val filePath = dirPath.resolve(fileName)
          val prog = SourceCode.fromFile(filePath.toString).exp
          val res = Interpreter(prog)
          println(fileName)
          println({
            Value.toString(res)
          })
          println()
        }
      case "compile" =>
        if (args.length < 3) {
          raise("usage: `run compile <path> <sdql_files>*`")
        }
        val dirPath = Path.of(args(1))
        for (fileName <- args.drop(2)) {
          val filePath = dirPath.resolve(fileName)
          val prog = SourceCode.fromFile(filePath.toString).exp
          val res = CppCodeGenerator(prog)
          println(fileName)
          println(compile(filePath.toString, res))
          println()
        }
      case arg =>
        raise(s"`run $arg` not supported")
    }
  }

  private def compile(sdqlFilePath: String, cpp: String) = {
    val cppFilePath = sdqlFilePath.replace(".sdql", ".cpp").replace("progs", "generated")
    reflect.io.File(cppFilePath).writeAll(cpp)
    format(cppFilePath)
  }

  private def format(cppFilePath: String) = Seq(
    "clang-format", "-i", cppFilePath, "-style", "{ColumnLimit: 120, IndentWidth: 4}"
  ).!!
}
