package sdql
package driver

import sdql.backend._
import sdql.frontend._
import sdql.ir._

import scala.sys.process._

object Main {

  def main(args: Array[String]): Unit = {
    if(args.length < 1) {
      raise("usage: `run <cmd> <args>*`")
    }
    args(0) match {
      case "interpret" =>
        if(args.length != 2) {
          raise("usage: `run interpret <sdql_file>`")
        }
        val sdqlFilePath = args(1)
        val prog = SourceCode.fromFile(sdqlFilePath).exp
        val res = Interpreter(prog)
        println(Value.toString(res))
      case "compile" =>
        if(args.length != 2) {
          raise("usage: `run compile <sdql_file>`")
        }
        val sdqlFilePath = args(1)
        val prog = SourceCode.fromFile(sdqlFilePath).exp
        val res = Compiler(prog)
        println(compile(sdqlFilePath, res))
      case arg =>
        raise(s"`run $arg` not supported")
    }
  }

  private def compile(sdqlFilePath: String, cpp: String) = {
    val noExtension = reFilename.findAllIn(sdqlFilePath).matchData.next().group(2)
    reflect.io.File(mk_cpp(noExtension)).writeAll(cpp)
    clang_format(noExtension)
    clang(noExtension)
    val res = run(noExtension)
    clean(noExtension)
    res
  }
  private def clang_format(noExtension: String) = Seq(
    "clang-format", "-i", mk_cpp(noExtension), "-style", "{ColumnLimit: 120}"
  ).!!
  private def clang(noExtension: String) = Seq(
    "clang++", "--std", "c++20", mk_cpp(noExtension), "-o", mk_out(noExtension)
  ).!!
  private def run(noExtension: String) = s"./${mk_out(noExtension)}".!!
  private def clean(noExtension: String) = s"rm ${mk_out(noExtension)}".!!
  private def mk_cpp(noExtension: String) = s"compiled/$noExtension.cpp"
  private def mk_out(noExtension: String) = s"compiled/$noExtension.out"
  private val reFilename = "^(.+/)*(.+)\\.(.+)$".r
}