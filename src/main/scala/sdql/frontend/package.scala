package sdql
import sdql.ir.*

package object frontend {
  implicit class Interpolator(val sc: StringContext) {
    def valueToString(v: Any): String = Value.toString(v)
    def sdql(args: Any*): Exp         = {
      val strings     = sc.parts.iterator
      val expressions = args.iterator
      val buf         = new StringBuffer(strings.next())
      while (strings.hasNext) {
        val nextExpression = expressions.next() match {
          case b: Boolean     => valueToString(b)
          case s: String      => valueToString(s)
          case d: Double      => valueToString(d)
          case i: Int         => valueToString(i)
          case m: Map[?, ?]   => valueToString(m)
          case r: RecordValue => valueToString(r)
          case Sym(name)      => name
          case x              => raise(s"doesn't know how to splice `$x`")
        }
        buf.append(nextExpression)
        buf.append(strings.next())
      }
      Parser(buf.toString)
    }
  }
}
