package sdql
import ir._

package object frontend {
  implicit class Interpolator(val sc: StringContext) {
    // val sdqlCG = new BaseDocument {} 
    def sdql(args: Any*): Exp = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        val nextExpression = expressions.next match {
          case s: String => s
          case d: Double => d.toString
          case i: Int => i.toString
          // case t: Exp => 
          //   val snippet = sdqlCG(t)
          //   t match {
          //     case s: Sym => snippet
          //     case _ => s"($snippet)"
          //   }
          case x =>
            raise(s"doesn't know how to splice `$x`")
        }
        buf.append(nextExpression)
        buf.append(strings.next)
      }
      Parser(buf.toString)
    }
  }
}
