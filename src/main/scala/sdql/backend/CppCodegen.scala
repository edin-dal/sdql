package sdql.backend

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.ir.ExternalFunctions.*
import sdql.raise

import scala.PartialFunction.{ cond, condOpt }

object CppCodegen {
  type TypesCtx = TypeInference.Ctx

  /** Generates C++ from an expression transformed to LLQL */
  def apply(e: Exp, benchmarkRuns: Int = 0): String = {
    val csvBody    = cppCsvs(e)
    val queryBody  = run(e)(Map(), isTernary = false)
    val benchStart =
      if (benchmarkRuns == 0) ""
      // note: first benchmark run is warmup
      else
        s"""HighPrecisionTimer timer;
           |for (${cppType(IntType)} iter = 0; iter <= $benchmarkRuns; iter++) {
           |timer.Reset();
           |""".stripMargin
    val benchStop  =
      if (benchmarkRuns == 0) cppPrintResult(TypeInference(e))
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
      case LetBinding(x @ Sym(name), e1, e2) =>
        val isTernary = !cond(e1) { case _: Sum | _: Initialise => true }
        val e1Cpp     = e1 match {
          // codegen for loads was handled in a separate tree traversal
          case _: Load                                 => ""
          case e1 @ External(ConstantString.SYMBOL, _) =>
            s"const auto $name = ${run(e1)(typesCtx, isTernary)};"
          case e1: Const                               => s"constexpr auto $name = ${run(e1)(Map(), isTernary = false)};"
          case _                                       =>
            val isRetrieval = cond(e1) { case _: FieldNode | _: Get => true }
            def isDict      = cond(TypeInference.run(e1)) { case _: DictType => true }
            val cppName     = if (isRetrieval && isDict) s"&$name" else name
            val semicolon   = if (cond(e1) { case _: Initialise => true }) "" else ";"
            s"auto $cppName = ${run(e1)(typesCtx, isTernary)}$semicolon"
        }
        val e2Cpp     = e2 match {
          case DictNode(Nil, _) => ""
          case _                => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), isTernary = false)
        }
        e1Cpp + e2Cpp

      case Sum(k, v, e1, e2) =>
        val (_, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
        val body            = CppCodegen.run(e2)(typesLocal, isTernary)
        val head            = e1 match {
          case _: RangeNode => s"${cppType(IntType)} ${k.name} = 0; ${k.name} < ${CppCodegen.run(e1)}; ${k.name}++"
          case _            =>
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

      case IfThenElse(a, Const(false), Const(true)) => s"!(${run(a)})"
      case IfThenElse(a, b, Const(false))           => s"(${run(a)} && ${run(b)})"
      case IfThenElse(a, Const(true), b)            => s"(${run(a)} || ${run(b)})"
      case e: IfThenElse if isTernary               => ternary(e)
      case e: IfThenElse                            => default(e)

      case Cmp(e1, e2: Sym, "∈")                                           =>
        TypeInference.run(e2) match {
          case _: DictType => dictCmpNil(e2, e1)
          case tpe         =>
            raise(
              s"expression ${e2.simpleName} should be of type " +
                s"${DictType.getClass.getSimpleName.init} not ${tpe.prettyPrint}"
            )
        }
      case Cmp(e @ Get(e1, e2), DictNode(Nil, _), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
      case Cmp(DictNode(Nil, _), e @ Get(e1, e2), "!=") if getArgsMatch(e) => dictCmpNil(e1, e2)
      case Cmp(e1, e2, cmp)                                                => s"${run(e1)} $cmp ${run(e2)}"

      case FieldNode(e1, field) =>
        val tpe = (TypeInference.run(e1): @unchecked) match { case rt: RecordType => rt }
        val idx = (tpe.indexOf(field): @unchecked) match { case Some(idx) => idx }
        s" /* $field */ std::get<$idx>(${run(e1)})"

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

      case Mult(_: Promote, _) | Mult(_, _: Promote) =>
        raise(s"promotion not supported for ${Mult.getClass.getSimpleName.init}")
      case Mult(e1, External(Inv.SYMBOL, Seq(e2)))   => s"(${run(e1)} / ${run(e2)})"
      case Mult(e1, e2)                              => s"(${run(e1)} * ${run(e2)})"

      case e: Neg => s"-${run(e)}"

      case Const(DateValue(v)) =>
        val yyyymmdd = "^(\\d{4})(\\d{2})(\\d{2})$".r.findAllIn(v.toString).matchData.next()
        s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
      case Const(v: String)    => s""""$v""""
      case Const(v)            => v.toString

      case Sym(name) => name

      case DictNode(Nil, _) => ""
      case DictNode(seq, _) =>
        seq.map { case (e1, e2) =>
          val e1Cpp = run(e1)(typesCtx, isTernary = true)
          val e2Cpp = run(e2)(typesCtx, isTernary = true)
          s"{$e1Cpp, $e2Cpp}"
        }
          .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")

      case RecNode(values) =>
        val tpe = TypeInference.run(e)
        values.map(e => run(e._2)(typesCtx, isTernary = true)).mkString(s"${cppType(tpe)}(", ", ", ")")

      case Get(e1, e2) =>
        (TypeInference.run(e1): @unchecked) match {
          case _: RecordType => s"std::get<${run(e2)}>(${run(e1)})"
          case _: DictType   => s"${run(e1)}[${run(e2)}]"
        }

      case External(ConstantString.SYMBOL, Seq(Const(str: String), Const(maxLen: Int))) =>
        assert(maxLen == str.length + 1)
        s"""ConstantString("$str", $maxLen)"""
      case External(StrContains.SYMBOL, Seq(str, subStr))                               =>
        val func = ((TypeInference.run(str), TypeInference.run(subStr)): @unchecked) match {
          case (StringType(None), StringType(None))                                              => "find"
          case (StringType(Some(_)), StringType(Some(_)))                                        => "contains"
          case (StringType(None), StringType(Some(_))) | (StringType(Some(_)), StringType(None)) =>
            raise(s"${StrContains.SYMBOL} doesn't support fixed and variable length strings together")
        }
        s"${CppCodegen.run(str)}.$func(${CppCodegen.run(subStr)})"
      case External(StrStartsWith.SYMBOL, Seq(str, prefix))                             =>
        val startsWith = (TypeInference.run(str): @unchecked) match {
          case StringType(None)    => "starts_with"
          case StringType(Some(_)) => "startsWith"
        }
        s"${CppCodegen.run(str)}.$startsWith(${CppCodegen.run(prefix)})"
      case External(StrEndsWith.SYMBOL, Seq(str, suffix))                               =>
        val endsWith = (TypeInference.run(str): @unchecked) match {
          case StringType(None)    => "ends_with"
          case StringType(Some(_)) => "endsWith"
        }
        s"${CppCodegen.run(str)}.$endsWith(${CppCodegen.run(suffix)})"
      case External(SubString.SYMBOL, Seq(str, Const(start: Int), Const(end: Int)))     =>
        val subStr = (TypeInference.run(str): @unchecked) match {
          case StringType(None)    => "substr"
          case StringType(Some(_)) => s"substr<${end - start}>"
        }
        s"${CppCodegen.run(str)}.$subStr($start, $end)"
      case External(StrIndexOf.SYMBOL, Seq(field: FieldNode, elem, from))               =>
        assert(cond(TypeInference.run(field)) { case StringType(None) => true })
        s"${CppCodegen.run(field)}.find(${CppCodegen.run(elem)}, ${CppCodegen.run(from)})"
      case External(FirstIndex.SYMBOL, Seq(on, patt))                                   =>
        s"${CppCodegen.run(on)}.firstIndex(${CppCodegen.run(patt)})"
      case External(LastIndex.SYMBOL, Seq(on, patt))                                    =>
        s"${CppCodegen.run(on)}.lastIndex(${CppCodegen.run(patt)})"
      case External(name @ Inv.SYMBOL, _)                                               =>
        raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")
      case External(Size.SYMBOL, Seq(arg))                                              =>
        TypeInference.run(arg) match {
          case _: DictType => s"${CppCodegen.run(arg)}.size()"
          case t           => raise(s"unexpected: ${t.prettyPrint}")
        }
      case External(name, _)                                                            => raise(s"unhandled function name: $name")

      case Concat(e1: RecNode, e2: RecNode) => run(e1.concat(e2))
      case Concat(e1: Sym, e2: Sym)         => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1: Sym, e2: RecNode)     => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1: RecNode, e2: Sym)     => s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1, e2)                   =>
        val _ = TypeInference.run(e)
        raise(
          s"${Concat.getClass.getSimpleName} requires arguments " +
            s"${RecNode.getClass.getSimpleName.init} and/or ${Sym.getClass.getSimpleName.init}, " +
            s"not ${e1.simpleName} and ${e2.simpleName}"
        )

      case Promote(_: TropicalSemiRingType, e) => run(e)
      case Promote(tp, e)                      => s"(${cppType(tp)})${run(e)}"

      case Unique(e) => run(e)

      case RangeNode(e) => run(e)

      // LLQL

      case Initialise(tpe, e) =>
        val unpacked      = TropicalSemiRingType.unpack(tpe)
        val agg           = Aggregation.fromType(tpe)
        val initialiseCpp = initialise(unpacked)(agg, typesCtx, isTernary)
        s"${cppType(unpacked)}($initialiseCpp); ${CppCodegen.run(e)}"

      case Update(e, agg, destination) =>
        val (lhs, rhs) = cppLhsRhs(e, destination)
        agg match {
          case SumAgg => s"$lhs += $rhs;"
          case MaxAgg => s"max_inplace($lhs, $rhs);"
          case MinAgg => s"min_inplace($lhs, $rhs);"
          case agg    => raise(s"$agg not supported")
        }

      case Modify(e, destination) =>
        val (lhs, rhs) = cppLhsRhs(e, destination)
        s"$lhs = $rhs;"

      case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }

  private def ternary(e: IfThenElse)(implicit typesCtx: TypesCtx)                     = e match {
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

  private def getArgsMatch(e: Get)(implicit typesCtx: TypesCtx) = e match {
    case Get(e1, e2) => cond(TypeInference.run(e1)) { case DictType(kt, _, _) => TypeInference.run(e2) == kt }
  }

  private def dictCmpNil(e1: Exp, e2: Exp)(implicit typesCtx: TypesCtx, isTernary: Boolean) =
    TypeInference.run(e1) match {
      case DictType(IntType, _, _: Vec) => s"${run(e1)}[${run(e2)}] != 0"
      case _                            => s"${run(e1)}.contains(${run(e2)})"
    }

  private def cppLhsRhs(e: Exp, destination: Sym)(implicit typesCtx: TypesCtx)                   = {
    val (accessors, inner) = splitNested(e)
    val bracketed          = cppAccessors(accessors)(typesCtx, isTernary = true)
    val lhs                = s"${destination.name}$bracketed"
    val rhs                = CppCodegen.run(inner)(typesCtx, isTernary = true)
    (lhs, rhs)
  }
  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, isTernary: Boolean) =
    exps.map(e => s"[${CppCodegen.run(e)}]").mkString("")
  private def splitNested(e: Exp): (Seq[Exp], Exp)                                               = e match {
    case DictNode(Seq((k, v @ DictNode(_, _: PHmap | _: SmallVecDict | _: SmallVecDicts))), _) =>
      val (lhs, rhs) = splitNested(v)
      (Seq(k) ++ lhs, rhs)
    case DictNode(Seq((k, DictNode(Seq((rhs, Const(1))), _: Vec))), _)                         => (Seq(k), rhs)
    case DictNode(Seq((k, rhs)), _)                                                            => (Seq(k), rhs)
    case DictNode(map, _) if map.length != 1                                                   => raise(s"unsupported: $e")
    case _                                                                                     => (Seq(), e)
  }

  private def initialise(tpe: Type)(implicit agg: Aggregation, typesCtx: TypesCtx, isTernary: Boolean): String =
    tpe match {
      case DictType(_, _, PHmap(Some(e)))                => CppCodegen.run(e)
      case DictType(_, _, PHmap(None) | _: SmallVecDict) => "{}"
      case DictType(_, _, Vec(size))                     =>
        size match {
          case None       => ""
          case Some(size) => (size + 1).toString
        }
      case DictType(_, _, _: SmallVecDicts)              => ""
      case RecordType(attrs)                             => attrs.map(_.tpe).map(initialise).mkString(", ")
      case BoolType                                      =>
        agg match {
          case SumAgg | MaxAgg  => "false"
          case ProdAgg | MinAgg => "true"
        }
      case RealType                                      =>
        agg match {
          case SumAgg | MaxAgg => "0.0"
          case ProdAgg         => "1.0"
          case MinAgg          => s"std::numeric_limits<${cppType(RealType)}>::max()"
        }
      case IntType | DateType                            =>
        agg match {
          case SumAgg | MaxAgg => "0"
          case ProdAgg         => "1"
          case MinAgg          => s"std::numeric_limits<${cppType(IntType)}>::max()"
        }
      case StringType(None)                              =>
        agg match {
          case SumAgg | MaxAgg => "\"\""
          case ProdAgg         => raise("undefined")
          case MinAgg          => s"MAX_STRING"
        }
      case StringType(Some(_))                           => raise("initialising VarChars shouldn't be needed")
      case tpe                                           => raise(s"unimplemented type: $tpe")
    }

  private def cppType(tpe: Type, noTemplate: Boolean = false): String = tpe match {
    case DictType(kt, vt, _: PHmap)                             =>
      val template = if (noTemplate) "" else s"<${cppType(kt)}, ${cppType(vt)}>"
      s"phmap::flat_hash_map$template"
    case DictType(kt, IntType, SmallVecDict(size))              =>
      val template = if (noTemplate) "" else s"<${cppType(kt)}, $size>"
      s"smallvecdict$template"
    case DictType(rt: RecordType, IntType, SmallVecDicts(size)) =>
      val template = if (noTemplate) "" else s"<$size, ${recordParams(rt)}>"
      s"smallvecdicts$template"
    case DictType(IntType, vt, _: Vec)                          =>
      val template = if (noTemplate) "" else s"<${cppType(vt)}>"
      s"std::vector$template"
    case rt: RecordType                                         =>
      val template = if (noTemplate) "" else s"<${recordParams(rt)}>"
      s"std::tuple$template"
    case BoolType                                               => "bool"
    case RealType                                               => "double"
    case IntType | DateType                                     => "int"
    case StringType(None)                                       => "std::string"
    case StringType(Some(maxLen))                               => s"VarChar<$maxLen>"
    case tpe                                                    => raise(s"unimplemented type: $tpe")
  }
  private def recordParams(rt: RecordType)                            = rt match {
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType(_)).mkString(", ")
  }

  // In the generated C++ program, CSVs are loaded into const variables outside the main function.
  // This is convenient so we can just time everything inside main for benchmarks (which shouldn't include load times).
  // Though it assumes loads expressions are bound to global variables (always holds for all of our queries).
  // E.g. we currently don't support cases like this one:
  //     if (...) then
  //         let same_varname = load[...]("foo.csv")
  //     else
  //         let same_varname = load[...]("bar.csv")
  private def cppCsvs(e: Exp): String                                                    = {
    val pathNameTypeSkip = iterExps(e).flatMap(extract).toSeq.distinct.sortBy(_._2)
    val csvConsts        =
      pathNameTypeSkip.zipWithIndex.map { case ((path, name, _, _), i) => makeCsvConst(name, path, i) }
        .mkString("\n", "\n", "\n")
    val tuples           = pathNameTypeSkip.map { case (_, name, recordType, skipCols) =>
      val init = makeTupleInit(name, recordType, skipCols)
      s"auto ${name.toLowerCase} = ${cppType(recordType, noTemplate = true)}($init);\n"
    }
      .mkString("\n")
    Seq(csvConsts, tuples).mkString("\n")
  }
  private def extract(e: Exp)                                                            = condOpt(e) {
    case LetBinding(Sym(name), load @ Load(path, tp: RecordType, _), _) if TypeInference.isColumnStore(tp) =>
      val recordType            = (load: @unchecked) match { case Load(_, recordType: RecordType, _) => recordType }
      val skipCols: Set[String] =
        (load: @unchecked) match { case Load(_, _, skipCols) => skipCols.toSkipColsSet }
      (path, name, recordType, skipCols)
  }
  private def makeCsvConst(name: String, path: String, i: Int) =
    // NaN handling for LSQB queries (different value in each table avoids joining them)
    s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR, IntNanConverter($i));"""
  private def makeTupleInit(name: String, recordType: RecordType, skipCols: Set[String]) = {
    assert(recordType.attrs.last.name == "size")
    val attrs    = recordType.attrs
      .dropRight(1)
      .map(attr => (attr.tpe: @unchecked) match { case DictType(IntType, vt, Vec(None)) => Attribute(attr.name, vt) })
    val readCols = attrs.zipWithIndex.filter { case (attr, _) => !skipCols.contains(attr.name) }.map {
      case (Attribute(attr_name, tpe), i) =>
        s"/* $attr_name */" ++ (tpe match {
          case DateType                 =>
            s"dates_to_numerics(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
          case StringType(Some(maxLen)) =>
            s"strings_to_varchars<$maxLen>(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
          case _                        =>
            s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i)"
        })
    }
    val readSize =
      if (skipCols.contains("size")) Seq()
      else Seq(s"/* size */static_cast<${cppType(IntType)}>(${name.toUpperCase}_CSV.GetRowCount())")
    (readCols ++ readSize).mkString(",\n")
  }
  private def iterExps(e: Exp): Iterator[Exp]                                            =
    Iterator(e) ++ (e match {
      case Restage(cs, _) => cs.flatMap(iterExps)
    })

  private def cppPrintResult(tpe: Type): String        = tpe match {
    case DictType(kt, vt, _: PHmap)        =>
      s"""for (const auto &[key, val] : $resultName) {
         |$stdCout << ${_cppPrintResult(kt, "key")} << ":" << ${_cppPrintResult(vt, "val")} << std::endl;
         |}""".stripMargin
    case DictType(kt, vt, _: SmallVecDict) =>
      assert(vt == IntType, s"${vt.simpleName} != ${IntType.simpleName}")
      val cond = vt match {
        case _: DictType   => s"!$resultName[i].empty()"
        case _: RecordType => raise(s"Print not implemented for ${vt.simpleName} inside vector")
        case t             =>
          assert(t.isScalar)
          s"$resultName[i] != 0"
      }
      s"""for (auto i = 0; i < $resultName.size(); ++i) {
         |if ($cond) {
         |$stdCout << ${_cppPrintResult(kt, "i")} << ":" << ${_cppPrintResult(vt, s"$resultName[i]")} << std::endl;
         |}
         |}""".stripMargin
    case _                                 => s"$stdCout << ${_cppPrintResult(tpe, resultName)} << std::endl;"
  }
  private def _cppPrintResult(tpe: Type, name: String) = tpe match {
    case _: DictType       => name // we currently don't pretty print inside nested dicts
    case RecordType(Nil)   => name
    case RecordType(attrs) =>
      attrs.zipWithIndex.map {
        case (Attribute(_, tpe: RecordType), _) => raise(s"Nested ${tpe.simpleName} not supported")
        case (Attribute(_, DateType), i)        => s"print_date(std::get<$i>($name))"
        case (Attribute(_, _), i)               => s"std::get<$i>($name)"
      }
        .mkString(""""<" <<""", """ << "," << """, """<< ">"""")
    case _                 =>
      assert(tpe.isScalar)
      name
  }
  private val stdCout                                  = s"std::cout << std::setprecision (std::numeric_limits<double>::digits10)"
}
