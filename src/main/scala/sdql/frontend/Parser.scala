package sdql
package frontend

import ir._
import fastparse._, NoWhitespace._, CharPredicates._


object Parser {
  def keywords[_: P] = P (
    StringIn("if", "then", "else", "let", "sum", "false", 
      "true", "in", "join", "load", "ext", "iter", "int", "double", 
      "string", "date", "range", "unit", "bool",
      "dense_int") ~
      !idRest
  )  
  def `false`[_: P]       = P( "false" ).map(_ => Const(false))
  def `true`[_: P]        = P( "true" ).map(_ => Const(true))
  def unit[_: P]          = P( "unit" ).map(_ => Const(()))
  def singleComment[_: P] = P( "//" ~/ (!newline ~ AnyChar).rep ~/ newline)
  def multiComment[_: P]  = P( "/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")
  def comment[_: P]       = P( multiComment | singleComment )
  def newline[_: P]       = P( "\n" | "\r\n" | "\r" )
  def whitespace[_: P]    = P( " " | "\t" | newline)
  def spaceToken[_: P]    = P( comment | whitespace.rep(1) )
  def space[_: P]         = P( spaceToken.rep )
  def digits[_: P]        = P( CharsWhileIn("0-9") )
  def exponent[_: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  def fractional[_: P]    = P( "." ~ digits )
  def integral[_: P]      = P( "0" | CharIn("1-9")  ~ digits.? )

  def int[_: P] = P(  CharIn("+\\-").? ~ integral ).!.map(
    x => Const(x.toInt)
  )

  def number[_: P] = P(  CharIn("+\\-").? ~ integral ~ fractional ~ exponent.? ).!.map(
    x => Const(x.toDouble)
  )

  def denseInt[_:P] = P( "dense_int" ~ "(" ~ (integral.!.map(_.toInt)) ~ space ~ "," ~ space ~/ (( "-1" | integral ).!.map(_.toInt)) ~ ")" ).map(
    x => Const(DenseInt(x._1, x._2))
  )

  def stringChars(c: Char) = c != '\"' && c != '\\'
  def hexDigit[_: P]      = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[_: P] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[_: P]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )
  def alpha[_: P]         = P( CharPred(isLetter) )

  def tpeBool[_: P]       = P( "bool" ).map(_ => BoolType)
  def tpeInt[_: P]        = P( "int" ).map(_ => IntType)
  def tpeReal[_: P]       = P( "double" | "real" ).map(_ => RealType)
  def tpeString[_: P]     = P( "string" ).map(_ => StringType)
  def tpeDate[_: P]       = P( "date" ).map(_ => DateType)
  def tpeIndex[_:P]       = P( "dense_int" ~ ("[" ~ space ~/ ( "-1" | integral ).!.map(_.toInt) ~/ space ~/ "]").?  ).map(x => DenseIntType(x.getOrElse(-1)))
  def fieldTpe[_: P]      = P( variable ~/ ":" ~ space ~/ tpe ).map(x => Attribute(x._1.name, x._2))
  def tpeRec[_: P]        =
    P( "<" ~/ fieldTpe.rep(sep=","./) ~ space ~/ ">").map(l => RecordType(l))
  def tpeDict[_: P]       = P( "{" ~/ tpe ~ space ~ "->" ~ space ~/ tpe ~ "}").map(x => DictType(x._1, x._2))
  def tpe[_: P]: P[Type]  = tpeBool | tpeInt | tpeReal | tpeString | tpeDate | tpeRec | tpeDict | tpeIndex

  def strChars[_: P] = P( CharsWhile(stringChars) )

  def string[_: P] =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Const)
  def fieldChars[_: P] = P( CharsWhile(_ != '`') )
  def fieldConst[_: P] =
    P( space ~ "`" ~/ (fieldChars | escape).rep.! ~ "`").map(x => Const(Symbol(x)))
  def const[_: P]: P[Const] = P(`true` | `false` | unit | number | int | string | denseInt)
  def idRest[_: P]: P[Char] = P( CharPred(c => isLetter(c) | isDigit(c) | c == '_').! ).map(_(0))
  def variable[_: P]: P[Sym] = P( space ~ !keywords ~ ((alpha | "_" | "$") ~ idRest.rep).! ~ space ).map(x => Sym(x))
  def ifThenElse[_: P]: P[IfThenElse] = P( "if" ~/ expr ~/ "then" ~/ expr ~/ "else" ~/ expr).map(x => IfThenElse(x._1, x._2, x._3))
  def letBinding[_: P]: P[LetBinding] = P( "let" ~/ variable ~/ "=" ~/ expr ~/ "in".? ~/ expr).map(x => LetBinding(x._1, x._2, x._3))
  def sum[_: P]: P[Sum] = P( "sum" ~ space ~/ "(" ~/ "<" ~/ variable ~/ "," ~/ variable ~/ ">" ~/ space ~/ ("<-" | "in") ~/ expr ~/ ")" ~/
    expr).map(x => Sum(x._1, x._2, x._3, x._4))
  // def set[_: P]: P[DictNode] = P( "{" ~/ (expr ~ !("->")).rep(sep=","./) ~ space ~/ "}" ).map(x => SetNode(x))
  def range[_: P]: P[RangeNode] = P( ("range(" ~ int ~ space ~ ")") ).map(x => RangeNode(x.v.asInstanceOf[Int]))
  def ext[_: P]: P[External] = P( "ext(" ~/ fieldConst ~/ "," ~/ expr.rep(1, sep=","./) ~ space ~/ ")" ).map(x =>
    External(x._1.v.asInstanceOf[Symbol].name, x._2))
  def keyValue[_: P] = P( expr ~/ "->" ~/ expr )
  // def keyNoValue[_: P] = P( expr ~ !("->") ).map(x => (x, Const(true)))
  def dict[_: P]: P[DictNode] =
    P( "{" ~/ keyValue.rep(sep=","./) ~ space ~/ "}").map(x => DictNode(x))
  def load[_: P]: P[Load] =
    P( "load" ~/ "[" ~/ tpe ~ space ~/ "]" ~/ "(" ~/ string ~/ ")").map(x => Load(x._2.v.asInstanceOf[String], x._1))
  // def set[_: P]: P[DictNode] =
  //   P( "{" ~/ keyNoValue.rep(sep=","./) ~ space ~/ "}").map(x => DictNode(x))
  // def dictOrSet[_: P]: P[DictNode] =
  //   P( "{" ~/ (keyNoValue | keyValue).rep(sep=","./) ~ space ~/ "}").map(x => DictNode(x))
  // def dictOrSet[_: P]: P[DictNode] = P( dict | set ) 
  def dictOrSet[_: P]: P[DictNode] = P( dict )
  
  def fieldValue[_: P] = P( variable ~/ "=" ~/ expr ).map(x => (x._1.name, x._2))
  def rec[_: P] =
    P( "<" ~/ fieldValue.rep(sep=","./) ~ space ~/ ">").map(x => RecNode(x))

  def factor[_: P]: P[Exp] = P(space ~ (const | neg | not | dictOrSet | 
    rec | ifThenElse | range | load |
    letBinding | sum | variable |
    ext | parens) ~ space)

  def neg[_: P]: P[Neg] = P( "-" ~ !(">") ~ factor ).map(Neg)
  def not[_: P]: P[Exp] = P( "!" ~ factor ).map(Not.apply)
  def factorMult[_: P]: P[Exp] = P( factor ~ (
      (".".! ~/ variable) | 
      ("^".! ~/ factor) | 
      ("(".! ~/ expr ~/ ")" ~ space)).rep ).map(x => 
        x._2.foldLeft(x._1)((acc, cur) => cur match {
            case (".", Sym(name)) => FieldNode(acc, name) 
            case ("(", e) => Get(acc, e)
            case ("^", e) => cur._2 match {
              case Const(2) => Mult(acc, acc)
              case _ => raise("Parsing for power failed")
            }
            case _ => raise("Parsing for factorMult failed")
          } ))
  def divMul[_: P]: P[Exp] = P( factorMult ~ (StringIn("*", "/", "|", "&&", "||").! ~/ factorMult).rep ).map(x => 
    x._2.foldLeft(x._1)((acc, cur) => cur._1 match {
      case "*" => Mult(acc, cur._2)
      case "/" => Mult(acc, External.Inv(cur._2))
      case "&&" => And(acc, cur._2)
      case "||" => Or(acc, cur._2)
    } ))
  // def addSubCmp[_: P]: P[Exp] = P( divMul ~ (StringIn("+", "-", "<", "==", "<=", ">=", ">", "!=").! ~ !(">") ~/ divMul).rep ).map(x => 
  //   x._2.foldLeft(x._1)((acc, cur) => cur._1 match {
  //     case "+" => Add(acc, cur._2)
  //     case "-" => Add(acc, Neg(cur._2))
  //     case op => Cmp(acc, cur._2, op)
  //   } ))
  def addSub[_: P]: P[Exp] = P( divMul ~ (StringIn("+", "-").! ~ !(">") ~/ divMul).rep ).map(x => 
    x._2.foldLeft(x._1)((acc, cur) => cur._1 match {
      case "+" => Add(acc, cur._2)
      case "-" => Add(acc, Neg(cur._2))
    } ))
  def addSubCmp[_: P]: P[Exp] = P( addSub ~ (StringIn("<", "==", "<=", "!=").! ~/ addSub).? ).map(x => 
      x._2 match {
        case Some((op, y)) => Cmp(x._1, y, op)
        case None => x._1
      }
  )
  def parens[_: P]: P[Exp] = P( "(" ~/ expr ~/ ")")
  def expr[_: P]: P[Exp] = P( addSubCmp )
  def top[_: P]: P[Exp] = P(expr ~ End)

  def apply(str: String): Exp = {
    val value = parse(str, top(_)) match {
      case Parsed.Success(value, _) => value
      case f @ Parsed.Failure(msg, p, extra) => raise(s"Parse failed `$f` for sdql`$str`")
    }
    value
  }

}
