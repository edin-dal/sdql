package sdql
package driver

import sdql.backend._
import sdql.frontend._
import sdql.ir._

import java.nio.file.Path

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
        val fileNames = args.drop(2)
        for (fileName <- fileNames) {
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
        val fileNames = args.drop(2)
        CppCompile.cmake(dirPath, fileNames)
        for (fileName <- fileNames) {
          val filePath = dirPath.resolve(fileName)
          val prog = SourceCode.fromFile(filePath.toString).exp
          val res = CppCodegen(prog)
          println(fileName)
          println(CppCompile.compile(filePath.toString, res))
          println()
        }
      case "benchmark" =>
        if(args.length < 3) {
          raise("usage: `run benchmark <path> <sdql_files>*`")
        }
        val dirPath = Path.of(args(1))
        val fileNames = args.drop(2)
        for (fileName <- fileNames) {
          val filePath = dirPath.resolve(fileName)
          val prog = SourceCode.fromFile(filePath.toString).exp
          val res = CppCodegen(prog, isBenchmark = true)
          CppCompile.writeFormat(filePath.toString, res)
        }
      case arg =>
        raise(s"`run $arg` not supported")
    }
  }
}
