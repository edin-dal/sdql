package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.ir.ExternalFunctions.*
import sdql.raise

import scala.PartialFunction.cond

object ExternalUtils {
  def run(e: External)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case External(ConstantString.SYMBOL, Seq(Const(str: String), Const(maxLen: Int))) =>
      assert(maxLen == str.length + 1)
      s"""ConstantString("$str", $maxLen)"""

    case External(StrContains.SYMBOL, Seq(str, subStr)) =>
      val func = ((TypeInference.run(str), TypeInference.run(subStr)): @unchecked) match {
        case (StringType(None), StringType(None))       => "find"
        case (StringType(Some(_)), StringType(Some(_))) => "contains"
        case (StringType(None), StringType(Some(_))) | (StringType(Some(_)), StringType(None)) =>
          raise(s"${StrContains.SYMBOL} doesn't support fixed and variable length strings together")
      }
      s"${CppCodegen.run(str)}.$func(${CppCodegen.run(subStr)})"

    case External(StrStartsWith.SYMBOL, Seq(str, prefix)) =>
      val startsWith = (TypeInference.run(str): @unchecked) match {
        case StringType(None)    => "starts_with"
        case StringType(Some(_)) => "startsWith"
      }
      s"${CppCodegen.run(str)}.$startsWith(${CppCodegen.run(prefix)})"

    case External(StrEndsWith.SYMBOL, Seq(str, suffix)) =>
      val endsWith = (TypeInference.run(str): @unchecked) match {
        case StringType(None)    => "ends_with"
        case StringType(Some(_)) => "endsWith"
      }
      s"${CppCodegen.run(str)}.$endsWith(${CppCodegen.run(suffix)})"

    case External(SubString.SYMBOL, Seq(str, Const(start: Int), Const(end: Int))) =>
      val subStr = (TypeInference.run(str): @unchecked) match {
        case StringType(None)    => "substr"
        case StringType(Some(_)) => s"substr<${end - start}>"
      }
      s"${CppCodegen.run(str)}.$subStr($start, $end)"

    case External(StrIndexOf.SYMBOL, Seq(field: FieldNode, elem, from)) =>
      assert(cond(TypeInference.run(field)) { case StringType(None) => true })
      s"${CppCodegen.run(field)}.find(${CppCodegen.run(elem)}, ${CppCodegen.run(from)})"

    case External(FirstIndex.SYMBOL, Seq(on, patt)) =>
      s"${CppCodegen.run(on)}.firstIndex(${CppCodegen.run(patt)})"

    case External(LastIndex.SYMBOL, Seq(on, patt)) =>
      s"${CppCodegen.run(on)}.lastIndex(${CppCodegen.run(patt)})"

    case External(name @ Inv.SYMBOL, _) =>
      raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")

    case External(Size.SYMBOL, Seq(arg)) =>
      TypeInference.run(arg) match {
        case _: DictType => s"${CppCodegen.run(arg)}.size()"
        case t           => raise(s"unexpected: ${t.prettyPrint}")
      }

    case External(name, _) =>
      raise(s"unhandled function name: $name")
  }
}
