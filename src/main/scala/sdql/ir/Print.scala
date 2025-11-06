package sdql.ir

import scala.PartialFunction.cond

object Print {

  def pretty(e: Exp)(implicit indent: Int = 0, inline: Boolean = false): String = {
    val ws = if (inline) "" else (1 to indent).map(_ => "    ").mkString("")
    ws ++ (e match {
      case Const(v)                                 => v.toString
      case Sym(name)                                => name
      case Load(path, tp, _)                        => s"load[${pretty(tp)}](\"$path\")"
      case LetBinding(x, e1, e2)                    =>
        s"let ${pretty(x)(indent, inline = true)} = ${pretty(e1)(indent, inline = true)}\n${pretty(e2)}"
      case Timer(e)                                 => s"\ntimer\n\n${pretty(e)}"
      case Sum(key, value, e1, body)                =>
        val hasNestedSum = iterExps(body).exists(cond(_) { case _: Sum => true })
        val bodyPrint    =
          if (hasNestedSum) s"\n${pretty(body)(indent + 1)}" else s" ${pretty(body)(indent + 1, inline = true)}"
        s"sum(<${pretty(key)(indent, inline = true)}, ${pretty(value)(indent, inline = true)}> <- ${pretty(e1)(indent, inline = true)})$bodyPrint"
      case RangeNode(exp)                           => s"range(${pretty(exp)})"
      case IfThenElse(cond, thenp, _)               => // TODO else
        s"if (${pretty(cond)(indent, inline = true)}) then\n${pretty(thenp)(indent + 1)}"
      case Cmp(e1, e2, cmp)                         => s"${pretty(e1)} $cmp ${pretty(e2)}"
      case DictNode(Seq((k, v)), hint)              =>
        s"${pretty(hint)}{ ${pretty(k)(indent, inline = true)} -> ${pretty(v)(indent, inline = true)} }"
      case RecNode(values)                          =>
        values.map { case (field, exp) => s"$field=${pretty(exp)}" }.mkString("<", ", ", ">")
      case Get(e1, e2)                              => s"${pretty(e1)}(${pretty(e2)})"
      case FieldNode(e, f)                          => s"${pretty(e)}.$f"
      case Promote(tsrt: TropicalSemiRingType, exp) =>
        s"promote[${pretty(tsrt)}](${pretty(exp)(indent, inline = true)})"
      case _                                        => e.toString
    })
  }

  private def pretty(tpe: Type): String = tpe match {
    case RecordType(attrs)          => attrs.map(pretty).mkString("<", ", ", ">")
    case DictType(key, value, hint) => s"${pretty(hint)}{${pretty(key)} -> ${pretty(value)}}"
    case StringType(None)           => "string"
    case StringType(Some(_))        => ???
    case RealType                   => "double"
    case BoolType                   => "bool"
    case IntType                    => "int"
    case LongType                   => "long"
    case DateType                   => "date"
    case _                          => tpe.toString
  }

  private def pretty(tsrt: TropicalSemiRingType): String = tsrt match {
    case TropicalSemiRingType(isMax, isProd, None) => TropicalSemiRingType.name(isMax, isProd)
    case _                                         => ???
  }

  private def pretty(hint: DictHint): String = hint match {
    case PHmap(None) => ""
    case _           => s"${_pretty(hint)} "
  }

  private def _pretty(hint: DictHint): String = "@" ++ (hint match {
    case PHmap(None)           => "phmap"
    case PHmap(Some(exp))      => s"phmap(${pretty(exp)})"
    case Range                 => "range"
    case SmallVecDict(size)    => s"smallvecdict($size)"
    case SmallVecDicts(size)   => s"smallvecdicts($size)"
    case SortedDict(None)      => "st"
    case SortedDict(Some(exp)) => s"st(${pretty(exp)})"
    case Vec(None)             => "vec"
    case Vec(Some(size))       => s"vec($size)"
  })

  private def pretty(attr: Attribute): String = attr match {
    case Attribute(name, tpe) => s"$name: ${pretty(tpe)}"
  }

  private def iterExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (e match {
      case Restage(cs, _) => cs.flatMap(iterExps)
    })
}
