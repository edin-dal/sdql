package sdql
package ir

/**
 * The type-class interface of the Restage design pattern.
 *
 * Refer to the following paper for more details: Amir Shaikhha, "Restaging Domain-Specific Languages: A Flexible Design
 * Pattern for Rapid Development of Optimizing Compilers", GPCE'24.
 */
trait Restage[T] {
  def restage(e: T): (Seq[T], Seq[T] => T) =
    children(e) -> factory(e)
  def children(e: T): Seq[T]
  def factory(e: T): Seq[T] => T
}

object Restage {
  type Fact[T] = Seq[T] => T
  def unapply[T: Restage](e: T): Some[(Seq[T], Seq[T] => T)] =
    Some(implicitly[Restage[T]].restage(e))

  implicit object RestageExp extends Restage[Exp] {
    def children(e: Exp): Seq[Exp]       = e match {
      // 0-ary
      case _: Sym | _: Const | _: Load => Seq()
      // 1-ary
      case Neg(e)                      => Seq(e)
      case FieldNode(e, _)             => Seq(e)
      case Promote(_, e)               => Seq(e)
      case RangeNode(e)                => Seq(e)
      case Unique(e)                   => Seq(e)
      // 2-ary
      case Add(e1, e2)                 => Seq(e1, e2)
      case Mult(e1, e2)                => Seq(e1, e2)
      case Cmp(e1, e2, _)              => Seq(e1, e2)
      case Sum(_, _, e1, e2)           => Seq(e1, e2)
      case Get(e1, e2)                 => Seq(e1, e2)
      case Concat(e1, e2)              => Seq(e1, e2)
      case LetBinding(_, e1, e2)       => Seq(e1, e2)
      // 3-ary
      case IfThenElse(e1, e2, e3)      => Seq(e1, e2, e3)
      // n-ary
      case RecNode(values)             => values.map(_._2)
      case DictNode(map, _)            => map.flatMap(x => Seq(x._1, x._2))
      case External(_, args)           => args
      case _                           => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }
    def factory(e: Exp): Seq[Exp] => Exp = e match {
      // 0-ary
      case _: Sym | _: Const | _: Load => _ => e
      // 1-ary
      case Neg(_)                      => seq => Neg(seq(0))
      case FieldNode(_, field)         => seq => FieldNode(seq(0), field)
      case Promote(tp, _)              => seq => Promote(tp, seq(0))
      case RangeNode(_)                => seq => RangeNode(seq(0))
      case Unique(_)                   => seq => Unique(seq(0))
      // 2-ary
      case Add(_, _)                 => seq => Add(seq(0), seq(1))
      case Mult(_, _)                => seq => Mult(seq(0), seq(1))
      case Cmp(_, _, cmp)            => seq => Cmp(seq(0), seq(1), cmp)
      case Sum(key, value, _, _)     => seq => Sum(key, value, seq(0), seq(1))
      case Get(_, _)                 => seq => Get(seq(0), seq(1))
      case Concat(_, _)              => seq => Concat(seq(0), seq(1))
      case LetBinding(x, _, _)       => seq => LetBinding(x, seq(0), seq(1))
      // 3-ary
      case IfThenElse(_, _, _)      => seq => IfThenElse(seq(0), seq(1), seq(2))
      // n-ary
      case RecNode(values)             => seq => RecNode(values.zip(seq).map(vs => (vs._1._1, vs._2)))
      case DictNode(map, hint)         =>
        seq => DictNode((0 until map.length).map(i => seq(i * 2) -> seq(i * 2 + 1)).toSeq, hint)
      case External(name, _)        => seq => External(name, seq)
      case _                           => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }
  }
}
