package sdql
package driver

import sdql.backend._
import sdql.frontend._
import sdql.ir._

import java.nio.file.{Path, Paths}

object Main {

  def main(args: Array[String]): Unit = {
    if(args.length < 1) {
      raise("usage: `run <cmd> <args>*`")
    }
    args(0) match {
      case "interpret" =>
        if(args.length < 3) {
          raise("usage: `run interpret <path> <sdql_files>*`")
        }
        val dirPath = Path.of(args(1))
        for (fileName <- args.drop(2)) {
          val filePath = dirPath.resolve(fileName)
          val prog = SourceCode.fromFile(filePath.toString).exp
          val res = Interpreter(prog)
          println(fileName)
          println({Value.toString(res)})
          println()
        }
      case "compile" =>
        if(args.length < 3) {
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

  // clang-format -i q1.cpp -style="{ColumnLimit: 120}"
  // && clang++ -std=c++20 q1.cpp
  // && ./q1.out
  // && rm q1.out
  private def compile(sdqlFilePath: String, cpp: String) = {
    val noExtension = reFilename.findAllIn(sdqlFilePath).matchData.next().group(2)
    reflect.io.File(cppPath(noExtension).toString).writeAll(cpp)
    in_generated(clang_format(noExtension)).!!
    in_generated(clang(noExtension)).!!
    val res = in_generated(run(noExtension)).!!
    in_generated(clean(noExtension)).!!
    res
  }
  private def clang_format(noExtension: String) = Seq(
    "clang-format", "-i", s"$noExtension.cpp", "-style", "{ColumnLimit: 120}"
  )
  private def clang(noExtension: String) = Seq(
    "clang++", "--std", "c++20", s"$noExtension.cpp", "-o", s"$noExtension.out"
  )
  private def run(noExtension: String) = Seq(s"./$noExtension.out")
  private def clean(noExtension: String) = Seq("rm", s"$noExtension.out")
  private def in_generated(seq: Seq[String]) = sys.process.Process(seq, generatedDir)
  private def cppPath(noExtension: String) = Paths.get(generatedDir.toString, s"$noExtension.cpp")
  private val generatedDir = new java.io.File("generated")
  private val reFilename = "^(.+/)*(.+)\\.(.+)$".r
}
