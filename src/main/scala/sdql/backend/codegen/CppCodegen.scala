package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.ir.ExternalFunctions.{ ConstantString, Inv }
import sdql.raise
import sdql.transformations.Rewriter

import scala.PartialFunction.cond

object CppCodegen {
  def apply(e: Exp, benchmarkRuns: Int = 0): String = {
    val noLLQL    = Rewriter(e)
    val csvBody   = ReadUtils.cppCsvs(Seq(noLLQL))
    val rewrite   = Rewriter.toLLQL(noLLQL)
    val queryBody = run(rewrite)(Map(), isTernary = false)
    val benchStart =
      if (benchmarkRuns == 0) ""
      else
        s"""HighPrecisionTimer timer;
           |for (${cppType(IntType)} iter = 1; iter <= $benchmarkRuns; iter++) {
           |timer.Reset();
           |""".stripMargin
    val benchStop =
      if (benchmarkRuns == 0) PrintUtils.cppPrintResult(TypeInference(rewrite))
      else
        s"""timer.StoreElapsedTime(0);
           |doNotOptimiseAway($resultName);
           |std::cerr << "*" << " " << std::flush;
           |}
           |std::cout << std::endl << timer.GetMean(0) << " ms";
           |""".stripMargin
    s"""#include "../runtime/headers.h"
       |$csvBody
       |int main() {
       |$benchStart
       |$queryBody
       |$benchStop
       |}""".stripMargin
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, isTernary: Boolean): String =
    e match {
      case e: LetBinding => run(e)
      case e: Sum        => run(e)
      case e: IfThenElse => run(e)
      case e: Cmp        => run(e)
      case e: FieldNode  => run(e)
      case e: Add        => run(e)
      case e: Mult       => run(e)
      case e: Neg        => run(e)
      case e: Const      => run(e)
      case e: Sym        => run(e)
      case e: DictNode   => run(e)
      case e: RecNode    => run(e)
      case e: Get        => run(e)
      case e: External   => run(e)
      case e: Concat     => run(e)
      case e: Promote    => run(e)
      case e: Unique     => run(e)
      case e: RangeNode  => run(e)
      case e: Initialise => LLQLUtils.run(e)
      case e: Update     => LLQLUtils.run(e)
      case e: Modify     => LLQLUtils.run(e)
      case _             => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }

  private def run(e: LetBinding)(implicit typesCtx: TypesCtx): String = e match {
    case LetBinding(x @ Sym(name), e1, e2) =>
      val isTernary = !cond(e1) { case _: Sum | _: Initialise => true }
      val e1Cpp = e1 match {
        // codegen for loads was handled in a separate tree traversal
        case _: Load => ""
        case e1 @ External(ConstantString.SYMBOL, _) =>
          s"const auto $name = ${run(e1)(typesCtx, isTernary)};"
        case e1: Const => s"constexpr auto $name = ${run(e1)};"
        case _ =>
          val isRetrieval = cond(e1) { case _: FieldNode | _: Get => true }
          def isDict      = cond(TypeInference.run(e1)) { case _: DictType => true }
          val cppName     = if (isRetrieval && isDict) s"&$name" else name
          s"auto $cppName = ${run(e1)(typesCtx, isTernary)};"
      }
      val e2Cpp = e2 match {
        case DictNode(Nil, _) => ""
        case _                => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), isTernary = false)
      }
      e1Cpp + e2Cpp
  }

  def run(e: Sum)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Sum(k, v, e1, e2) =>
      val (_, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val body            = CppCodegen.run(e2)(typesLocal, isTernary)
      val head = e1 match {
        case _: RangeNode => s"${cppType(IntType)} ${k.name} = 0; ${k.name} < ${CppCodegen.run(e1)}; ${k.name}++"
        case _ =>
          val lhs = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, _: PHmap)         => s"&[${k.name}, ${v.name}]"
            case DictType(_, _, _: SmallVecDict)  => s"&${k.name}"
            case DictType(_, _, _: SmallVecDicts) => s"${k.name}"
            case t                                => raise(s"unexpected: ${t.prettyPrint}")
          }
          val rhs = CppCodegen.run(e1)(typesLocal, isTernary)
          s"auto $lhs : $rhs"
      }
      s"for ($head) { $body }"
  }

  private def run(e: IfThenElse)(implicit typesCtx: TypesCtx, isTernary: Boolean): String =
    e match {
      case IfThenElse(a, Const(false), Const(true)) => s"!(${run(a)})"
      case IfThenElse(a, b, Const(false))           => s"(${run(a)} && ${run(b)})"
      case IfThenElse(a, Const(true), b)            => s"(${run(a)} || ${run(b)})"
      case _ if isTernary                           => ternary(e)
      case _                                        => default(e)
    }
  private def ternary(e: IfThenElse)(implicit typesCtx: TypesCtx) = e match {
    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, isTernary = true)
      val ifBody   = run(e1)(typesCtx, isTernary = true)
      val elseBody = run(e2)(typesCtx, isTernary = true)
      s"($condBody) ? $ifBody : $elseBody"
  }
  private def default(e: IfThenElse)(implicit typesCtx: TypesCtx, isTernary: Boolean) = e match {
    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, isTernary)
      val ifBody   = run(e1)
      val elseBody = e2 match {
        case DictNode(Nil, _) | Update(DictNode(Nil, _), _, _) | Const(0) | Const(0.0) => ""
        case _                                                                         => s" else {\n${run(e2)}\n}"
      }
      s"if ($condBody) {$ifBody\n}$elseBody"
  }

  private def run(e: Cmp)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Cmp(e1, e2: Sym, "∈") =>
      TypeInference.run(e2) match {
        case _: DictType => dictCmpNil(e2, e1)
        case tpe =>
          raise(
            s"expression ${e2.simpleName} should be of type " +
              s"${DictType.getClass.getSimpleName.init} not ${tpe.prettyPrint}"
          )
      }
    case Cmp(e @ Get(e1, e2), DictNode(Nil, _), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
    case Cmp(DictNode(Nil, _), e @ Get(e1, e2), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
    case Cmp(e1, e2, cmp)                                                => s"${run(e1)} $cmp ${run(e2)}"
  }
  private def getArgsMatch(e: Get)(implicit typesCtx: TypesCtx) = e match {
    case Get(e1, e2) => cond(TypeInference.run(e1)) { case DictType(kt, _, _) => TypeInference.run(e2) == kt }
  }
  private def dictCmpNil(e1: Exp, e2: Exp)(implicit typesCtx: TypesCtx, isTernary: Boolean) =
    TypeInference.run(e1) match {
      case DictType(IntType, _, _: Vec) => s"${run(e1)}[${run(e2)}] != 0"
      case _                            => s"${run(e1)}.contains(${run(e2)})"
    }

  private def run(e: FieldNode)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case FieldNode(e1, field) =>
      val tpe = (TypeInference.run(e1): @unchecked) match { case rt: RecordType => rt }
      val idx = (tpe.indexOf(field): @unchecked) match { case Some(idx)         => idx }
      s" /* $field */ std::get<$idx>(${run(e1)})"
  }

  private def run(e: Add)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Add(Promote(tp1, e1), Promote(tp2, e2)) =>
      assert(tp1 == tp2)
      Aggregation.fromType(tp1) match {
        case MinAgg  => s"std::min(${run(e1)}, ${run(e2)})"
        case MaxAgg  => s"std::max(${run(e1)}, ${run(e2)})"
        case ProdAgg => s"${run(e1)} * ${run(e2)}"
        case SumAgg  => s"${run(e1)} + ${run(e2)}"
      }
    case Add(_: Promote, _) | Add(_, _: Promote) => raise(s"binary expression $e promoted on one side only")
    case Add(e1, Neg(e2))                        => s"(${run(e1)} - ${run(e2)})"
    case Add(e1, e2)                             => s"(${run(e1)} + ${run(e2)})"
  }

  private def run(e: Mult)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Mult(_: Promote, _) | Mult(_, _: Promote) =>
      raise(s"promotion not supported for ${Mult.getClass.getSimpleName.init}")
    case Mult(e1, External(Inv.SYMBOL, Seq(e2))) => s"(${run(e1)} / ${run(e2)})"
    case Mult(e1, e2)                            => s"(${run(e1)} * ${run(e2)})"
  }

  private def run(e: Neg)(implicit typesCtx: TypesCtx): String = s"-${run(e)}"

  private def run(e: Sym): String = e match { case Sym(name) => name }

  private def run(e: DictNode)(implicit typesCtx: TypesCtx): String = e match {
    case DictNode(Nil, _) => ""
    case DictNode(seq, _) =>
      seq
        .map({
          case (e1, e2) =>
            val e1Cpp = run(e1)(typesCtx, isTernary = true)
            val e2Cpp = run(e2)(typesCtx, isTernary = true)
            s"{$e1Cpp, $e2Cpp}"
        })
        .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")
  }

  private def run(e: RecNode)(implicit typesCtx: TypesCtx): String = e match {
    case RecNode(values) =>
      val tpe = TypeInference.run(e)
      values.map(e => run(e._2)(typesCtx, isTernary = true)).mkString(s"${cppType(tpe)}(", ", ", ")")
  }

  private def run(e: Get)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Get(e1, e2) =>
      (TypeInference.run(e1): @unchecked) match {
        case _: RecordType => s"std::get<${run(e2)}>(${run(e1)})"
        case _: DictType   => s"${run(e1)}[${run(e2)}]"
      }
  }

  private def run(e: External)(implicit typesCtx: TypesCtx, isTernary: Boolean): String =
    ExternalUtils.run(e)

  private def run(e: Concat)(implicit typesCtx: TypesCtx): String = e match {
    case Concat(e1: RecNode, e2: RecNode) => run(e1.concat(e2))
    case Concat(e1: Sym, e2: Sym)         => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1: Sym, e2: RecNode)     => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1: RecNode, e2: Sym)     => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
    case Concat(e1, e2) =>
      val _ = TypeInference.run(e)
      raise(
        s"${Concat.getClass.getSimpleName} requires arguments " +
          s"${RecNode.getClass.getSimpleName.init} and/or ${Sym.getClass.getSimpleName.init}, " +
          s"not ${e1.simpleName} and ${e2.simpleName}"
      )
  }

  private def run(e: Const) = e match {
    case Const(DateValue(v)) =>
      val yyyymmdd = "^(\\d{4})(\\d{2})(\\d{2})$".r.findAllIn(v.toString).matchData.next()
      s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
    case Const(v: String) => s""""$v""""
    case Const(v)         => v.toString
  }

  private def run(e: Promote)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Promote(_: TropicalSemiRingType, e) => run(e) // handled in sum
    case Promote(tp, e)                      => s"(${cppType(tp)})${run(e)}"
  }

  private def run(e: Unique)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Unique(e) => run(e)
  }

  private def run(e: RangeNode)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case RangeNode(e) => run(e)
  }
}
