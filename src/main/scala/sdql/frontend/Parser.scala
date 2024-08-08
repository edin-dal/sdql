package sdql
package frontend

import fastparse.CharPredicates._
import fastparse.NoWhitespace._
import fastparse._
import sdql.ir._

object Parser {
  def keywords(implicit ctx: P[_]) = P(
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
      "nullable",
      "dense_int"
    ) ~
      !idRest
  )
  def `false`(implicit ctx: P[_]) = P("false").map(_ => Const(false))
  def `true`(implicit ctx: P[_]) = P("true").map(_ => Const(true))
  def unit(implicit ctx: P[_]) = P("unit").map(_ => Const(()))
  def singleComment(implicit ctx: P[_]) = P("//" ~/ (!newline ~ AnyChar).rep ~/ newline)
  def multiComment(implicit ctx: P[_]) = P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")
  def comment(implicit ctx: P[_]) = P(multiComment | singleComment)
  def newline(implicit ctx: P[_]) = P("\n" | "\r\n" | "\r")
  def whitespace(implicit ctx: P[_]) = P(" " | "\t" | newline)
  def spaceToken(implicit ctx: P[_]) = P(comment | whitespace.rep(1))
  def space(implicit ctx: P[_]) = P(spaceToken.rep)
  def digits(implicit ctx: P[_]) = P(CharsWhileIn("0-9"))
  def exponent(implicit ctx: P[_]) = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional(implicit ctx: P[_]) = P("." ~ digits)
  def integral(implicit ctx: P[_]) = P("0" | CharIn("1-9") ~ digits.?)

  def int(implicit ctx: P[_]) = P(CharIn("+\\-").? ~ integral).!.map(
    x => Const(x.toInt)
  )

  def number(implicit ctx: P[_]) = P(CharIn("+\\-").? ~ integral ~ fractional ~ exponent.?).!.map(
    x => Const(x.toDouble)
  )

  def denseInt(implicit ctx: P[_]) =
    P("dense_int" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ "," ~ space ~/ (("-1" | integral).!.map(_.toInt)) ~ ")")
      .map(
        x => Const(DenseInt(x._1, x._2))
      )
  def dateValue(implicit ctx: P[_]) = P("date" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ ")").map(
    x => Const(DateValue(x))
  )
  def concat(implicit ctx: P[_]) = P("concat" ~ "(" ~ expr ~ space ~ "," ~ space ~/ expr ~ ")").map(
    x => Concat(x._1, x._2)
  )

  def stringChars(c: Char) = c != '\"' && c != '\\'
  def hexDigit(implicit ctx: P[_]) = P(CharIn("0-9a-fA-F"))
  def unicodeEscape(implicit ctx: P[_]) = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape(implicit ctx: P[_]) = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
  def alpha(implicit ctx: P[_]) = P(CharPred(isLetter))

  def tpeTropSR(implicit ctx: P[_]) =
    P(
      ("mnpr" | "mxpr" | "mnsm" | "mxsm" |
        "min_prod" | "max_prod" | "min_sum" | "max_sum").!).map(x => TropicalSemiRingType(x))
  def tpeEnum(implicit ctx: P[_]) =
    P("enum" ~ ("[" ~ space ~/ tpe ~/ space ~/ "]")).map(x => EnumSemiRingType(x))
  def tpeNullable(implicit ctx: P[_]) =
    P("nullable" ~ ("[" ~ space ~/ tpe ~/ space ~/ "]")).map(x => NullableSemiRingType(x))
  def tpeBool(implicit ctx: P[_]) = P("bool").map(_ => BoolType)
  def tpeInt(implicit ctx: P[_]) = P("int").map(_ => IntType)
  def tpeReal(implicit ctx: P[_]) = P("double" | "real").map(_ => RealType)
  def tpeString(implicit ctx: P[_]) = P("string").map(_ => StringType())
  def tpeVarChar(implicit ctx: P[_]) = P("varchar" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ ")").map(
    x => VarCharType(x)
  )
  def tpeDate(implicit ctx: P[_]) = P("date").map(_ => DateType)
  def tpeIndex(implicit ctx: P[_]) =
    P("dense_int" ~ ("[" ~ space ~/ ("-1" | integral).!.map(_.toInt) ~/ space ~/ "]").?).map(x =>
      DenseIntType(x.getOrElse(-1)))
  def fieldTpe(implicit ctx: P[_]) = P(variable ~/ ":" ~ space ~/ tpe).map(x => Attribute(x._1.name, x._2))
  def tpeRec(implicit ctx: P[_]) =
    P("<" ~/ fieldTpe.rep(sep = ","./) ~ space ~/ ">").map(l => RecordType(l))
  def tpeDict(implicit ctx: P[_]) = P(hinted.? ~ tpeDictNoHint).map {
    case (Some(hint), DictType(kt, vt, _)) => DictType(kt, vt, hint)
    case (None, dict)                      => dict
  }
  def tpeDictNoHint(implicit ctx: P[_]) =
    P("{" ~/ tpe ~ space ~ "->" ~ space ~/ tpe ~ "}").map(x => DictType(x._1, x._2))
  def tpe(implicit ctx: P[_]): P[Type] =
    tpeBool | tpeInt | tpeReal | tpeString | tpeVarChar | tpeDate | tpeRec | tpeDict | tpeIndex | tpeTropSR | tpeEnum | tpeNullable

  def strChars(implicit ctx: P[_]) = P(CharsWhile(stringChars))

  def string(implicit ctx: P[_]) =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Const)
  def fieldChars(implicit ctx: P[_]) = P(CharsWhile(_ != '`'))
  def fieldConst(implicit ctx: P[_]) =
    P(space ~ "`" ~/ (fieldChars | escape).rep.! ~ "`").map(x => Const(Symbol(x)))
  def const(implicit ctx: P[_]): P[Const] = P(`true` | `false` | unit | number | int | string | denseInt | dateValue)
  def idRest(implicit ctx: P[_]): P[Char] = P(CharPred(c => isLetter(c) | isDigit(c) | c == '_').!).map(_(0))
  def variable(implicit ctx: P[_]): P[Sym] =
    P(space ~ !keywords ~ ((alpha | "_" | "$") ~ idRest.rep).! ~ space).map(x => Sym(x))
  def ifThenElse(implicit ctx: P[_]): P[IfThenElse] = P(ifThen ~/ maybeElse.?).map {
    case (cond: Exp, thenp: Exp, Some(elsep: Exp)) => IfThenElse(cond, thenp, elsep)
    case (cond: Exp, thenp: Exp, None)             => IfThenElse(cond, thenp, DictNode(Nil))
  }
  def ifThen(implicit ctx: P[_]): P[(Exp, Exp)] = P("if" ~/ expr ~/ "then" ~/ expr)
  def maybeElse(implicit ctx: P[_]): P[Exp] = P("else" ~/ expr)
  def letBinding(implicit ctx: P[_]): P[LetBinding] =
    P("let" ~/ variable ~/ "=" ~/ expr ~/ "in".? ~/ expr).map(x => LetBinding(x._1, x._2, x._3))
  def sum(implicit ctx: P[_]): P[Sum] =
    P(
      "sum" ~ space ~/ "(" ~/ "<" ~/ variable ~/ "," ~/ variable ~/ ">" ~/ space ~/ ("<-" | "in") ~/ expr ~/ ")" ~/
        expr).map(x => Sum(x._1, x._2, x._3, x._4))
  // def set(implicit ctx: P[_]): P[DictNode] = P( "{" ~/ (expr ~ !("->")).rep(sep=","./) ~ space ~/ "}" ).map(x => SetNode(x))
  def range(implicit ctx: P[_]): P[RangeNode] = P(("range(" ~ expr ~ space ~ ")")).map(RangeNode)
  def ext(implicit ctx: P[_]): P[External] =
    P("ext(" ~/ fieldConst ~/ "," ~/ expr.rep(1, sep = ","./) ~ space ~/ ")").map(x =>
      External(x._1.v.asInstanceOf[Symbol].name, x._2))
  def keyValue(implicit ctx: P[_]) = P(expr ~/ "->" ~/ expr)
  // def keyNoValue(implicit ctx: P[_]) = P( expr ~ !("->") ).map(x => (x, Const(true)))
  def dict(implicit ctx: P[_]): P[DictNode] = P(hinted.? ~ dictNoHint).map {
    case (Some(hint), DictNode(map, _)) => DictNode(map, hint)
    case (None, dict)                   => dict
  }
  def dictNoHint(implicit ctx: P[_]): P[DictNode] =
    P("{" ~/ keyValue.rep(sep = ","./) ~ space ~/ "}").map(x => DictNode(x))
  def hinted(implicit ctx: P[_]) = P("@" ~/ hint ~/ space)
  def hint(implicit ctx: P[_]) = phmap | vecdict | vec
  def phmap(implicit ctx: P[_]) = P("phmap").map(_ => NoHint)
  def vecdict(implicit ctx: P[_]) = P("vecdict").map(_ => VecDict)
  def vec(implicit ctx: P[_]) = P("vec").map(_ => Vec)
  def load(implicit ctx: P[_]): P[Load] =
    P("load" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ string ~/ ")").map(x => Load(x._2.v.asInstanceOf[String], x._1))
  def promote(implicit ctx: P[_]): P[Promote] =
    P("promote" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ expr ~/ ")").map(x => Promote(x._1, x._2))
  def unique(implicit ctx: P[_]): P[Unique] = P("unique" ~/ "(" ~/ expr ~/ ")").map(Unique)
  // def set(implicit ctx: P[_]): P[DictNode] =
  //   P( "{" ~/ keyNoValue.rep(sep=","./) ~ space ~/ "}").map(x => DictNode(x))
  // def dictOrSet(implicit ctx: P[_]): P[DictNode] =
  //   P( "{" ~/ (keyNoValue | keyValue).rep(sep=","./) ~ space ~/ "}").map(x => DictNode(x))
  // def dictOrSet(implicit ctx: P[_]): P[DictNode] = P( dict | set )
  def dictOrSet(implicit ctx: P[_]): P[DictNode] = P(dict)

  def fieldValue(implicit ctx: P[_]) = P(variable ~/ "=" ~/ expr).map(x => (x._1.name, x._2))
  def rec(implicit ctx: P[_]) =
    P("<" ~/ fieldValue.rep(sep = ","./) ~ space ~/ ">").map(x => RecNode(x))

  def factor(implicit ctx: P[_]): P[Exp] =
    P(
      space ~ (const | neg | not | dictOrSet |
        rec | ifThenElse | range | load | concat | promote | unique |
        letBinding | sum | variable |
        ext | parens) ~ space)

  def neg(implicit ctx: P[_]): P[Neg] = P("-" ~ !(">") ~ factor).map(Neg)
  def not(implicit ctx: P[_]): P[Exp] = P("!" ~ factor).map(Not.apply)
  def factorMult(implicit ctx: P[_]): P[Exp] =
    P(
      factor ~ ((".".! ~/ variable) |
        ("^".! ~/ factor) |
        ("(".! ~/ expr ~/ ")" ~ space)).rep).map(x =>
      x._2.foldLeft(x._1)((acc, cur) =>
        cur match {
          case (".", Sym(name)) => FieldNode(acc, name)
          case ("(", e)         => Get(acc, e)
          case ("^", e) =>
            cur._2 match {
              case Const(2) => Mult(acc, acc)
              case _        => raise("Parsing for power failed")
            }
          case _ => raise("Parsing for factorMult failed")
      }))
  def divMul(implicit ctx: P[_]): P[Exp] =
    P(factorMult ~ (StringIn("*", "/", "|", "&&", "||").! ~/ factorMult).rep).map(x =>
      x._2.foldLeft(x._1)((acc, cur) =>
        cur._1 match {
          case "*"  => Mult(acc, cur._2)
          case "/"  => Mult(acc, ExternalFunctions.Inv(cur._2))
          case "&&" => And(acc, cur._2)
          case "||" => Or(acc, cur._2)
      }))
  // def addSubCmp(implicit ctx: P[_]): P[Exp] = P( divMul ~ (StringIn("+", "-", "<", "==", "<=", ">=", ">", "!=").! ~ !(">") ~/ divMul).rep ).map(x =>
  //   x._2.foldLeft(x._1)((acc, cur) => cur._1 match {
  //     case "+" => Add(acc, cur._2)
  //     case "-" => Add(acc, Neg(cur._2))
  //     case op => Cmp(acc, cur._2, op)
  //   } ))
  def addSub(implicit ctx: P[_]): P[Exp] =
    P(divMul ~ (StringIn("+", "-").! ~ !(">") ~/ divMul).rep).map(x =>
      x._2.foldLeft(x._1)((acc, cur) =>
        cur._1 match {
          case "+" => Add(acc, cur._2)
          case "-" => Add(acc, Neg(cur._2))
      }))
  def addSubCmp(implicit ctx: P[_]): P[Exp] =
    P(addSub ~ (StringIn("<", "==", "<=", "!=", "∈").! ~/ addSub).?).map(x =>
      x._2 match {
        case Some((op, y)) => Cmp(x._1, y, op)
        case None          => x._1
    })
  def parens(implicit ctx: P[_]): P[Exp] = P("(" ~/ expr ~/ ")")
  def expr(implicit ctx: P[_]): P[Exp] = P(addSubCmp)
  def top(implicit ctx: P[_]): P[Exp] = P(expr ~ End)

  def apply(str: String): Exp = {
    val value = parse(str, top(_)) match {
      case Parsed.Success(value, _)          => value
      case f @ Parsed.Failure(msg, p, extra) => raise(s"Parse failed `$f` for sdql`$str`")
    }
    value
  }

}
