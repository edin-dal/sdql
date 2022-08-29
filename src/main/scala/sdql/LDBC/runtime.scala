package sdql.LDBC
import sdql.backend.Interpreter
import sdql.frontend.{Interpolator, SourceCode}
import sdql.ir.Value
import sdql.{backend, frontend, ir}
object runtime {
  def main(args: Array[String]): Unit = {
    val filename = "progs/LDBC/test.sdql"
    val prog = SourceCode.fromFile(filename).exp
    val startTime: Long = System.currentTimeMillis
    val res = Interpreter(prog)
    val endTime: Long = System.currentTimeMillis
    val mb = 1024*1024
    val runtime = Runtime.getRuntime
    println("runtime " + (endTime - startTime) + "ms")
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    println(Value.toString(res))
  }
}
