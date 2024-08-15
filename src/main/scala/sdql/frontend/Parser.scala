package sdql
package frontend

import fastparse.CharPredicates.*
import fastparse.NoWhitespace.*
import fastparse.*
import sdql.ir.*

object Parser {
  private def keywords(implicit ctx: P[?]) = P(
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
  private def `false`(implicit ctx: P[?])       = P("false").map(_ => Const(false))
  private def `true`(implicit ctx: P[?])        = P("true").map(_ => Const(true))
  private def unit(implicit ctx: P[?])          = P("unit").map(_ => Const(()))
  private def singleComment(implicit ctx: P[?]) = P("//" ~/ (!newline ~ AnyChar).rep ~/ newline)
  private def multiComment(implicit ctx: P[?])  = P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")
  private def comment(implicit ctx: P[?])       = P(multiComment | singleComment)
  private def newline(implicit ctx: P[?])       = P("\n" | "\r\n" | "\r")
  private def whitespace(implicit ctx: P[?])    = P(" " | "\t" | newline)
  private def spaceToken(implicit ctx: P[?])    = P(comment | whitespace.rep(1))
  private def space(implicit ctx: P[?])         = P(spaceToken.rep)
  private def digits(implicit ctx: P[?])        = P(CharsWhileIn("0-9"))
  private def exponent(implicit ctx: P[?])      = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  private def fractional(implicit ctx: P[?])    = P("." ~ digits)
  private def integral(implicit ctx: P[?])      = P("0" | CharIn("1-9") ~ digits.?)

  private def int(implicit ctx: P[?]) = P(CharIn("+\\-").? ~ integral).!.map(
    x => Const(x.toInt)
  )

  private def number(implicit ctx: P[?]) = P(CharIn("+\\-").? ~ integral ~ fractional ~ exponent.?).!.map(
    x => Const(x.toDouble)
  )

  private def denseInt(implicit ctx: P[?]) =
    P("dense_int" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ "," ~ space ~/ (("-1" | integral).!.map(_.toInt)) ~ ")")
      .map(
        x => Const(DenseInt(x._1, x._2))
      )
  private def dateValue(implicit ctx: P[?]) = P("date" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ ")").map(
    x => Const(DateValue(x))
  )
  private def concat(implicit ctx: P[?]) = P("concat" ~ "(" ~ expr ~ space ~ "," ~ space ~/ expr ~ ")").map(
    x => Concat(x._1, x._2)
  )

  private def stringChars(c: Char)              = c != '\"' && c != '\\'
  private def hexDigit(implicit ctx: P[?])      = P(CharIn("0-9a-fA-F"))
  private def unicodeEscape(implicit ctx: P[?]) = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  private def escape(implicit ctx: P[?])        = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
  private def alpha(implicit ctx: P[?])         = P(CharPred(isLetter))

  private def tpeTropSR(implicit ctx: P[?]) =
    P(("mnpr" | "mxpr" | "mnsm" | "mxsm" | "min_prod" | "max_prod" | "min_sum" | "max_sum").!)
      .map(TropicalSemiRingType(_))
  private def tpeEnum(implicit ctx: P[?]) =
    P("enum" ~ ("[" ~ space ~/ tpe ~/ space ~/ "]")).map(EnumSemiRingType.apply)
  private def tpeNullable(implicit ctx: P[?]) =
    P("nullable" ~ ("[" ~ space ~/ tpe ~/ space ~/ "]")).map(NullableSemiRingType.apply)
  private def tpeBool(implicit ctx: P[?])   = P("bool").map(_ => BoolType)
  private def tpeInt(implicit ctx: P[?])    = P("int").map(_ => IntType)
  private def tpeReal(implicit ctx: P[?])   = P("double" | "real").map(_ => RealType)
  private def tpeString(implicit ctx: P[?]) = P("string").map(_ => StringType())
  private def tpeVarChar(implicit ctx: P[?]) =
    P("varchar" ~ "(" ~ integral.!.map(_.toInt) ~ space ~ ")").map(VarCharType.apply)
  private def tpeDate(implicit ctx: P[?]) = P("date").map(_ => DateType)
  private def tpeIndex(implicit ctx: P[?]) =
    P("dense_int" ~ ("[" ~ space ~/ ("-1" | integral).!.map(_.toInt) ~/ space ~/ "]").?)
      .map(x => DenseIntType(x.getOrElse(-1)))
  private def fieldTpe(implicit ctx: P[?]) = P(variable ~/ ":" ~ space ~/ tpe).map(x => Attribute(x._1.name, x._2))
  private def tpeRec(implicit ctx: P[?]) =
    P("<" ~/ fieldTpe.rep(sep = ","./) ~ space ~/ ">").map(l => RecordType(l))
  private def tpeDict(implicit ctx: P[?]) = P(hinted.? ~ tpeDictNoHint).map {
    case (Some(hint), DictType(kt, vt, _)) => DictType(kt, vt, hint)
    case (None, dict)                      => dict
  }
  private def tpeDictNoHint(implicit ctx: P[?]) =
    P("{" ~/ tpe ~ space ~ "->" ~ space ~/ tpe ~ "}").map(x => DictType(x._1, x._2))
  private def tpe(implicit ctx: P[?]): P[Type] =
    tpeBool | tpeInt | tpeReal | tpeString | tpeVarChar | tpeDate | tpeRec | tpeDict | tpeIndex | tpeTropSR | tpeEnum | tpeNullable

  private def strChars(implicit ctx: P[?]) = P(CharsWhile(stringChars))

  private def string(implicit ctx: P[?]) =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Const.apply)
  private def fieldChars(implicit ctx: P[?]) = P(CharsWhile(_ != '`'))
  private def fieldConst(implicit ctx: P[?]) =
    P(space ~ "`" ~/ (fieldChars | escape).rep.! ~ "`").map(x => Const(Symbol(x)))
  private def const(implicit ctx: P[?])  = `true` | `false` | unit | number | int | string | denseInt | dateValue
  private def idRest(implicit ctx: P[?]) = P(CharPred(c => isLetter(c) | isDigit(c) | c == '_').!).map(_(0))
  private def variable(implicit ctx: P[?]) =
    P(space ~ !keywords ~ ((alpha | "_" | "$") ~ idRest.rep).! ~ space).map(Sym.apply)
  private def ifThenElse(implicit ctx: P[?]) = P(ifThen ~/ maybeElse.?).map {
    case (cond: Exp, thenp: Exp, Some(elsep: Exp)) => IfThenElse(cond, thenp, elsep)
    case (cond: Exp, thenp: Exp, None)             => IfThenElse(cond, thenp, DictNode(Nil))
  }
  private def ifThen(implicit ctx: P[?])    = P("if" ~/ expr ~/ "then" ~/ expr)
  private def maybeElse(implicit ctx: P[?]) = P("else" ~/ expr)
  private def letBinding(implicit ctx: P[?]) =
    P("let" ~/ variable ~/ "=" ~/ expr ~/ "in".? ~/ expr).map(x => LetBinding(x._1, x._2, x._3))
  private def sum(implicit ctx: P[?]) =
    P(
      "sum" ~ space ~/ "(" ~/ "<" ~/ variable ~/ "," ~/ variable ~/ ">" ~/ space ~/ ("<-" | "in") ~/ expr ~/ ")" ~/ expr
    ).map(x => Sum(x._1, x._2, x._3, x._4))
  private def range(implicit ctx: P[?]) = P(("range(" ~ expr ~ space ~ ")")).map(RangeNode.apply)
  private def ext(implicit ctx: P[?]) =
    P("ext(" ~/ fieldConst ~/ "," ~/ expr.rep(1, sep = ","./) ~ space ~/ ")")
      .map(x => External(x._1.v.asInstanceOf[Symbol].name, x._2))
  private def promote(implicit ctx: P[?]) =
    P("promote" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ expr ~/ ")").map(x => Promote(x._1, x._2))
  private def unique(implicit ctx: P[?])     = P("unique" ~/ "(" ~/ expr ~/ ")").map(Unique.apply)
  private def fieldValue(implicit ctx: P[?]) = P(variable ~/ "=" ~/ expr).map(x => (x._1.name, x._2))
  private def rec(implicit ctx: P[?])        = P("<" ~/ fieldValue.rep(sep = ","./) ~ space ~/ ">").map(RecNode.apply)

  private def load(implicit ctx: P[?]) =
    P("load" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ string ~/ skipCols.? ~ ")")
      .map(x => {
        val skipCols = x._3 match {
          case Some(cols) => cols
          case None       => SetNode(Nil)
        }
        Load(x._2.v.asInstanceOf[String], x._1, skipCols)
      })
  private def skipCols(implicit ctx: P[?]) = P("," ~ space ~ set)

  private def dictOrSet(implicit ctx: P[?])  = dict | set
  private def keyNoValue(implicit ctx: P[?]) = P(expr ~/ !"->")
  private def keyValue(implicit ctx: P[?])   = P(expr ~ "->" ~/ expr)
  private def set(implicit ctx: P[?])        = P("{" ~ keyNoValue.rep(sep = ",") ~ space ~ "}").map(SetNode.apply)
  private def dict(implicit ctx: P[?]) = P(hinted.? ~ dictNoHint).map {
    case (Some(hint), DictNode(map, _)) => DictNode(map, hint)
    case (None, dict)                   => dict
  }
  private def dictNoHint(implicit ctx: P[?])   = P("{" ~ keyValue.rep(sep = ","./) ~ space ~ "}").map(DictNode(_))
  private def hinted(implicit ctx: P[?])       = P("@" ~/ hint ~/ space)
  private def hint(implicit ctx: P[?])         = phmap | smallvecdict | vecdict | vec
  private def phmap(implicit ctx: P[?])        = P("phmap").map(_ => NoHint)
  private def smallvecdict(implicit ctx: P[?]) = P("smallvecdict" ~ sized).map(SmallVecDict.apply)
  private def vecdict(implicit ctx: P[?])      = P("vecdict").map(_ => VecDict)
  private def vec(implicit ctx: P[?])          = P("vec" ~ sized.?).map(Vec.apply)
  private def sized(implicit ctx: P[?])        = P("(" ~/ integral.!.map(_.toInt) ~/ ")")

  private def factor(implicit ctx: P[?]) =
    P(
      space ~ (const | neg | not | dictOrSet |
        rec | ifThenElse | range | load | concat | promote | unique |
        letBinding | sum | variable |
        ext | parens) ~ space
    )

  private def neg(implicit ctx: P[?]): P[Neg] = P("-" ~ !(">") ~ factor).map(Neg.apply)
  private def not(implicit ctx: P[?]): P[Exp] = P("!" ~ factor).map(Not.apply)
  private def factorMult(implicit ctx: P[?]) =
    P(
      factor ~ ((".".! ~/ variable) |
        ("^".! ~/ factor) |
        ("(".! ~/ expr ~/ ")" ~ space)).rep
    ).map(
      x =>
        x._2.foldLeft(x._1)(
          (acc, cur) =>
            cur match {
              case (".", Sym(name)) => FieldNode(acc, name)
              case ("(", e)         => Get(acc, e)
              case ("^", _) =>
                cur._2 match {
                  case Const(2) => Mult(acc, acc)
                  case _        => raise("Parsing for power failed")
                }
              case _ => raise("Parsing for factorMult failed")
          }
      )
    )
  private def divMul(implicit ctx: P[?]) =
    P(factorMult ~ (StringIn("*", "/", "|", "&&", "||").! ~/ factorMult).rep).map(
      x =>
        x._2.foldLeft(x._1)(
          (acc, cur) =>
            cur._1 match {
              case "*"  => Mult(acc, cur._2)
              case "/"  => Mult(acc, ExternalFunctions.Inv(cur._2))
              case "&&" => And(acc, cur._2)
              case "||" => Or(acc, cur._2)
          }
      )
    )
//  private def addSubCmp(implicit ctx: P[?]) =
//    P(divMul ~ (StringIn("+", "-", "<", "==", "<=", ">=", ">", "!=").! ~ !(">") ~/ divMul).rep).map(
//      x =>
//        x._2.foldLeft(x._1)(
//          (acc, cur) =>
//            cur._1 match {
//              case "+" => Add(acc, cur._2)
//              case "-" => Add(acc, Neg(cur._2))
//              case op  => Cmp(acc, cur._2, op)
//          }
//      )
//    )
  private def addSub(implicit ctx: P[?]) =
    P(divMul ~ (StringIn("+", "-").! ~ !(">") ~/ divMul).rep).map(
      x =>
        x._2.foldLeft(x._1)(
          (acc, cur) =>
            cur._1 match {
              case "+" => Add(acc, cur._2)
              case "-" => Add(acc, Neg(cur._2))
          }
      )
    )
  private def addSubCmp(implicit ctx: P[?]) =
    P(addSub ~ (StringIn("<", "==", "<=", "!=", "∈").! ~/ addSub).?).map(
      x =>
        x._2 match {
          case Some((op, y)) => Cmp(x._1, y, op)
          case None          => x._1
      }
    )
  private def parens(implicit ctx: P[?])       = P("(" ~/ expr ~/ ")")
  private def expr(implicit ctx: P[?]): P[Exp] = addSubCmp
  private def top(implicit ctx: P[?])          = P(expr ~ End)

  def apply(str: String): Exp = {
    val value = parse(str, top(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure        => raise(s"Parse failed `$f` for sdql`$str`")
    }
    value
  }
}
