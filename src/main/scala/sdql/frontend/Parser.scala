package sdql
package frontend

import fastparse.*
import fastparse.CharPredicates.*
import fastparse.NoWhitespace.*
import sdql.ir.*

object Parser {
  private def keywords[$: P]      = P(
    StringIn(
      "if",
      "then",
      "else",
      "let",
      "sum",
      "false",
      "true",
      "in",
      "join",
      "load",
      "ext",
      "iter",
      "int",
      "long",
      "double",
      "string",
      "varchar",
      "date",
      "range",
      "unit",
      "bool",
      "concat",
      "promote",
      "mnpr",
      "mxpr",
      "mnsm",
      "mxsm",
      "min_prod",
      "max_prod",
      "min_sum",
      "max_sum",
      "enum",
      "nullable"
    ) ~
      !idRest
  )
  private def `false`[$: P]       = P("false").map(_ => Const(false))
  private def `true`[$: P]        = P("true").map(_ => Const(true))
  private def unit[$: P]          = P("unit").map(_ => Const(()))
  private def singleComment[$: P] = P("//" ~/ (!newline ~ AnyChar).rep ~/ newline)
  private def multiComment[$: P]  = P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")
  private def comment[$: P]       = P(multiComment | singleComment)
  private def newline[$: P]       = P("\n" | "\r\n" | "\r")
  private def whitespace[$: P]    = P(" " | "\t" | newline)
  private def spaceToken[$: P]    = P(comment | whitespace.rep(1))
  private def space[$: P]         = P(spaceToken.rep)
  private def digits[$: P]        = P(CharsWhileIn("0-9"))
  private def exponent[$: P]      = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  private def fractional[$: P]    = P("." ~ digits)
  private def integral[$: P]      = P("0" | CharIn("1-9") ~ digits.?)
  private def real[$: P]          = P(CharIn("+\\-").? ~ integral ~ fractional ~ exponent.?).!.map(x => Const(x.toDouble))

  private def int[$: P]       = P(intHint.? ~ intNoHint).map {
    case (Some("long"), s) => Const(s.toLong)
    case (_, s)            => Const(s.toInt)
  }
  private def intHint[$: P]   = P("@" ~ intHints ~ space)
  private def intHints[$: P]  = ("int" | "long").!
  private def intNoHint[$: P] = P(sign.!.? ~ integral.!).map {
    case (Some("-"), s) => s"-$s"
    case (_, s)         => s
  }
  private def sign[$: P]      = "+" | "-"

  private def dateValue[$: P] = P("date" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ ")").map(x => Const(DateValue(x)))
  private def concat[$: P]    = P("concat" ~ "(" ~ expr ~ space ~ "," ~ space ~/ expr ~ ")").map(x => Concat(x._1, x._2))

  private def stringChars(c: Char) = c != '\"' && c != '\\'
  private def hexDigit[$: P]       = P(CharIn("0-9a-fA-F"))
  private def unicodeEscape[$: P]  = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  private def escape[$: P]         = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
  private def alpha[$: P]          = P(CharPred(isLetter))

  private def tpeTropSR[$: P]     =
    P(("mnpr" | "mxpr" | "mnsm" | "mxsm" | "min_prod" | "max_prod" | "min_sum" | "max_sum").!)
      .map(TropicalSemiRingType.apply)
  private def tpeEnum[$: P]       = P("enum" ~ ("[" ~ space ~/ tpe ~/ space ~/ "]")).map(EnumSemiRingType.apply)
  private def tpeNullable[$: P]   = P("nullable" ~ ("[" ~ space ~/ tpe ~/ space ~/ "]")).map(NullableSemiRingType.apply)
  private def tpeBool[$: P]       = P("bool").map(_ => BoolType)
  private def tpeInt[$: P]        = P("int").map(_ => IntType)
  private def tpeLong[$: P]       = P("long").map(_ => LongType)
  private def tpeReal[$: P]       = P("double" | "real").map(_ => RealType)
  private def tpeString[$: P]     = P("string").map(_ => StringType())
  private def tpeVarChar[$: P]    = P("varchar" ~ "(" ~ integral.!.map(_.toInt) ~ space ~ ")").map(VarCharType.apply)
  private def tpeDate[$: P]       = P("date").map(_ => DateType)
  private def fieldTpe[$: P]      = P(variable ~/ ":" ~ space ~/ tpe).map(x => Attribute(x._1.name, x._2))
  private def tpeRec[$: P]        = P("<" ~/ fieldTpe.rep(sep = ","./) ~ space ~/ ">").map(l => RecordType(l))
  private def tpeDict[$: P]       = P(hinted.? ~ tpeDictNoHint).map {
    case (Some(hint), DictType(kt, vt, _)) => DictType(kt, vt, hint)
    case (None, dict)                      => dict
  }
  private def tpeDictNoHint[$: P] = P("{" ~/ tpe ~ space ~ "->" ~ space ~/ tpe ~ "}").map(x => DictType(x._1, x._2))
  private def tpe[$: P]: P[Type]  =
    tpeBool | tpeInt | tpeLong | tpeReal | tpeString | tpeVarChar |
      tpeDate | tpeRec | tpeDict | tpeTropSR | tpeEnum | tpeNullable

  private def strChars[$: P] = P(CharsWhile(stringChars))

  private def string[$: P]     = P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Const.apply)
  private def fieldChars[$: P] = P(CharsWhile(_ != '`'))
  private def fieldConst[$: P] = P(space ~ "`" ~/ (fieldChars | escape).rep.! ~ "`").map(x => Const(Symbol(x)))
  private def const[$: P]      = `true` | `false` | unit | real | int | string | dateValue
  private def idRest[$: P]     = P(CharPred(c => isLetter(c) | isDigit(c) | c == '_').!).map(_(0))
  private def variable[$: P]   = P(space ~ !keywords ~ ((alpha | "_" | "$") ~ idRest.rep).! ~ space).map(Sym.apply)
  private def ifThenElse[$: P] = P(ifThen ~/ maybeElse.?).map {
    case (cond: Exp, thenp: Exp, Some(elsep: Exp)) => IfThenElse(cond, thenp, elsep)
    case (cond: Exp, thenp: Exp, None)             => IfThenElse(cond, thenp, DictNode(Nil))
  }
  private def ifThen[$: P]     = P("if" ~/ expr ~/ "then" ~/ expr)
  private def maybeElse[$: P]  = P("else" ~/ expr)
  private def letBinding[$: P] =
    P("let" ~/ variable ~/ "=" ~/ expr ~/ "in".? ~/ expr).map(x => LetBinding(x._1, x._2, x._3))
  private def sum[$: P]        =
    P(
      "sum" ~ space ~/ "(" ~/ "<" ~/ variable ~/ "," ~/ variable ~/ ">" ~/ space ~/ ("<-" | "in") ~/ expr ~/ ")" ~/ expr
    ).map(x => Sum(x._1, x._2, x._3, x._4))
  private def range[$: P]      = P(("range(" ~ expr ~ space ~ ")")).map(RangeNode.apply)
  private def ext[$: P]        =
    P("ext(" ~/ fieldConst ~/ "," ~/ expr.rep(1, sep = ","./) ~ space ~/ ")")
      .map(x => External(x._1.v.asInstanceOf[Symbol].name, x._2))
  private def promote[$: P]    =
    P("promote" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ expr ~/ ")").map(x => Promote(x._1, x._2))
  private def unique[$: P]     = P("unique" ~/ "(" ~/ expr ~/ ")").map(Unique.apply)
  private def fieldValue[$: P] = P(variable ~/ "=" ~/ expr).map(x => (x._1.name, x._2))
  private def rec[$: P]        = P("<" ~/ fieldValue.rep(sep = ","./) ~ space ~/ ">").map(RecNode.apply)

  private def load[$: P]     =
    P("load" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ string ~/ skipCols.? ~ ")").map { x =>
      val skipCols = x._3 match {
        case Some(cols) => cols
        case None       => SetNode(Nil)
      }
      Load(x._2.v.asInstanceOf[String], x._1, skipCols)
    }
  private def skipCols[$: P] = P("," ~ space ~ set)

  private def dictOrSet[$: P]     = dict | set
  private def keyNoValue[$: P]    = P(expr ~/ !"->")
  private def keyValue[$: P]      = P(expr ~ "->" ~/ expr)
  private def set[$: P]           = P("{" ~ keyNoValue.rep(sep = ",") ~ space ~ "}").map(SetNode.apply)
  private def dict[$: P]          = P(hinted.? ~ dictNoHint).map {
    case (Some(hint), DictNode(map, _)) => DictNode(map, hint)
    case (None, dict)                   => dict
  }
  private def dictNoHint[$: P]    = P("{" ~ keyValue.rep(sep = ","./) ~ space ~ "}").map(DictNode(_))
  private def hinted[$: P]        = P("@" ~/ hint ~/ space)
  private def hint[$: P]          = phmap | smallvecdicts | smallvecdict | sorteddict | vec
  private def phmap[$: P]         = P("phmap" ~ parens.?).map(PHmap.apply)
  private def smallvecdict[$: P]  = P("smallvecdict" ~ sized).map(SmallVecDict.apply)
  private def smallvecdicts[$: P] = P("smallvecdicts" ~ sized).map(SmallVecDicts.apply)
  private def sorteddict[$: P]    = P("st" ~ parens.?).map(SortedDict.apply)
  private def vec[$: P]           = P("vec" ~ sized.?).map(Vec.apply)
  private def sized[$: P]         = P("(" ~/ integral.!.map(_.toInt) ~/ ")")

  private def factor[$: P] =
    P(
      space ~ (const | neg | not | dictOrSet |
        rec | ifThenElse | range | load | concat | promote | unique |
        letBinding | sum | variable |
        ext | parens) ~ space
    )

  private def neg[$: P]: P[Neg]  = P("-" ~ !(">") ~ factor).map(Neg.apply)
  private def not[$: P]: P[Exp]  = P("!" ~ factor).map(Not.apply)
  private def factorMult[$: P]   =
    P(
      factor ~ ((".".! ~/ variable) |
        ("^".! ~/ factor) |
        ("(".! ~/ expr ~/ ")" ~ space)).rep
    ).map(x =>
      x._2.foldLeft(x._1)((acc, cur) =>
        cur match {
          case (".", Sym(name)) => FieldNode(acc, name)
          case ("(", e)         => Get(acc, e)
          case ("^", _)         =>
            cur._2 match {
              case Const(0)      => Const(1)
              case Const(1)      => acc
              case Const(n: Int) => (1 to n).map(_ => acc).reduceLeft(Mult.apply)
              case _             => raise("Parsing for power failed")
            }
          case _                => raise("Parsing for factorMult failed")
        }
      )
    )
  private def divMul[$: P]       =
    P(factorMult ~ (StringIn("*", "/", "|", "&&", "||").! ~/ factorMult).rep).map(x =>
      x._2.foldLeft(x._1)((acc, cur) =>
        cur._1 match {
          case "*"  => Mult(acc, cur._2)
          case "/"  => Mult(acc, ExternalFunctions.Inv(cur._2))
          case "&&" => And(acc, cur._2)
          case "||" => Or(acc, cur._2)
        }
      )
    )
  private def addSub[$: P]       =
    P(divMul ~ (StringIn("+", "-").! ~ !(">") ~/ divMul).rep).map(x =>
      x._2.foldLeft(x._1)((acc, cur) =>
        cur._1 match {
          case "+" => Add(acc, cur._2)
          case "-" => Add(acc, Neg(cur._2))
        }
      )
    )
  private def addSubCmp[$: P]    =
    P(addSub ~ (StringIn("<", "==", "<=", "!=", "∈").! ~/ addSub).?).map(x =>
      x._2 match {
        case Some((op, y)) => Cmp(x._1, y, op)
        case None          => x._1
      }
    )
  private def parens[$: P]       = P("(" ~/ expr ~/ ")")
  private def expr[$: P]: P[Exp] = addSubCmp
  private def top[$: P]          = P(expr ~ End)

  def apply(str: String): Exp = {
    val value = parse(str, top(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure        => raise(s"Parse failed `$f` for sdql`$str`")
    }
    value
  }
}
