package sdql
package driver
import frontend._
import backend._
import ir._

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
        val filename = args(1)
        val prog = SourceCode.fromFile(filename).exp
        val res = Interpreter(prog)
        println(Value.toString(res))
      case "compile" =>
        if(args.length != 2) {
          raise("usage: `run compile <sdql_file>`")
        }
        val filename = args(1)
        val prog = SourceCode.fromFile(filename).exp
        val res = Compiler(prog)
        println(Value.toString(res))
      case arg =>
        raise(s"`run $arg` not supported")
    }
  }
}