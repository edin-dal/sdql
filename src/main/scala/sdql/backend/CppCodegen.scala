package sdql.backend

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.ir.ExternalFunctions.*
import sdql.raise

import scala.PartialFunction.cond

object CppCodegen {
  type TypesCtx = TypeInference.Ctx

  /** Generates C++ from an expression transformed to LLQL */
  def apply(e: Exp, benchmarkRuns: Int = 0): String = {
    val queryBody = run(e)(Map(), isTernary = false, benchmarkRuns)
    s"""#include "../runtime/headers.h"\n
       |int main() {
       |$queryBody
       |}""".stripMargin
  }

  private def run(e: Exp)(implicit typesCtx: TypesCtx, isTernary: Boolean, benchmarkRuns: Int): String =
    e match {
      case LetBinding(x @ Sym(name), e1, e2) =>
        val isTernary = !cond(e1) { case _: Sum | _: Initialise => true }
        val e1Cpp     = e1 match {
          case e: Load                                                        => load(e, name)
          case e1 @ External(ConstantString.SYMBOL | SortedIndices.SYMBOL, _) =>
            s"const auto $name = ${run(e1)(typesCtx, isTernary, benchmarkRuns)};"
          case e1: Const                                                      =>
            s"constexpr auto $name = ${run(e1)(Map(), isTernary = false, benchmarkRuns)};"
          case _                                                              =>
            val isRetrieval = cond(e1) { case _: FieldNode | _: Get => true }
            def isDict      = cond(TypeInference.run(e1)) { case _: DictType => true }
            val cppName     = if (isRetrieval && isDict) s"&$name" else name
            val semicolon   = if (cond(e1) { case _: Initialise => true }) "" else ";"
            s"auto $cppName = ${run(e1)(typesCtx, isTernary, benchmarkRuns)}$semicolon"
        }
        val e2Cpp     = e2 match {
          case DictNode(Nil, _) => ""
          case _                => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), isTernary = false, benchmarkRuns)
        }
        e1Cpp + e2Cpp

      case Sum(k, v, e1, e2) =>
        val (_, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
        val body            = run(e2)(typesLocal, isTernary, benchmarkRuns)
        val head            = e1 match {
          case _: RangeNode => s"${cppType(IntType)} ${k.name} = 0; ${k.name} < ${run(e1)}; ${k.name}++"
          case _            =>
            val lhs = TypeInference.run(e1)(typesLocal) match {
              case DictType(_, _, _: PHmap | SortedDict(_)) => s"&[${k.name}, ${v.name}]"
              case DictType(_, _, _: SmallVecDict)          => s"&${k.name}"
              case DictType(_, _, _: SmallVecDicts | Range) => s"${k.name}"
              case DictType(_, _, hint)                     => raise(s"unexpected dictionary hint: $hint")
              case t                                        => raise(s"unexpected: ${t.prettyPrint}")
            }
            val rhs = run(e1)(typesLocal, isTernary, benchmarkRuns)
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

      case Neg(e) => s"-${run(e)}"

      case Const(DateValue(v)) =>
        val yyyymmdd = "^(\\d{4})(\\d{2})(\\d{2})$".r.findAllIn(v.toString).matchData.next()
        s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
      case Const(v: String)    => s""""$v""""
      case Const(v)            => v.toString

      case Sym(name) => name

      case DictNode(Nil, _) => ""
      case DictNode(seq, _) =>
        seq.map { case (e1, e2) =>
          val e1Cpp = run(e1)(typesCtx, isTernary = true, benchmarkRuns)
          val e2Cpp = run(e2)(typesCtx, isTernary = true, benchmarkRuns)
          s"{$e1Cpp, $e2Cpp}"
        }
          .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")

      case RecNode(values) =>
        val tpe = TypeInference.run(e)
        values.map(e => run(e._2)(typesCtx, isTernary = true, benchmarkRuns)).mkString(s"${cppType(tpe)}(", ", ", ")")

      case Get(e1, e2) =>
        (TypeInference.run(e1): @unchecked) match {
          case _: RecordType                 => s"std::get<${run(e2)}>(${run(e1)})"
          case DictType(_, _, _: SortedDict) => s"${run(e1)}.at(${run(e2)})"
          case _: DictType                   => s"${run(e1)}[${run(e2)}]"
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
        s"${run(str)}.$func(${run(subStr)})"
      case External(StrStartsWith.SYMBOL, Seq(str, prefix))                             =>
        val startsWith = (TypeInference.run(str): @unchecked) match {
          case StringType(None)    => "starts_with"
          case StringType(Some(_)) => "startsWith"
        }
        s"${run(str)}.$startsWith(${run(prefix)})"
      case External(StrEndsWith.SYMBOL, Seq(str, suffix))                               =>
        val endsWith = (TypeInference.run(str): @unchecked) match {
          case StringType(None)    => "ends_with"
          case StringType(Some(_)) => "endsWith"
        }
        s"${run(str)}.$endsWith(${run(suffix)})"
      case External(SubString.SYMBOL, Seq(str, Const(start: Int), Const(end: Int)))     =>
        val subStr = (TypeInference.run(str): @unchecked) match {
          case StringType(None)    => "substr"
          case StringType(Some(_)) => s"substr<${end - start}>"
        }
        s"${run(str)}.$subStr($start, $end)"
      case External(StrIndexOf.SYMBOL, Seq(field: FieldNode, elem, from))               =>
        assert(cond(TypeInference.run(field)) { case StringType(None) => true })
        s"${run(field)}.find(${run(elem)}, ${run(from)})"
      case External(FirstIndex.SYMBOL, Seq(on, patt))                                   =>
        s"${run(on)}.firstIndex(${run(patt)})"
      case External(LastIndex.SYMBOL, Seq(on, patt))                                    =>
        s"${run(on)}.lastIndex(${run(patt)})"
      case External(SortedIndices.SYMBOL, args)                                         =>
        s"sorted_indices(${args.map(run).mkString(", ")})"
      case External(SortedVec.SYMBOL, Seq(n, arg))                                      =>
        s"sort_vec<${run(n)}>(std::move(${run(arg)}))"
      case External(name @ Inv.SYMBOL, _)                                               =>
        raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")
      case External(Size.SYMBOL, Seq(arg))                                              =>
        TypeInference.run(arg) match {
          case _: DictType => s"static_cast<${cppType(IntType)}>(${run(arg)}.size())"
          case t           => raise(s"unexpected: ${t.prettyPrint}")
        }
      case External(name, _)                                                            =>
        raise(s"unhandled function name: $name")

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
        val initialiseCpp = initialise(unpacked)(agg, typesCtx, isTernary, benchmarkRuns)
        s"${cppType(unpacked)}($initialiseCpp); ${run(e)}"

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
        TypeInference.run(e) match {
          // avoiding tuple construction in emplace_back has negligible performance gains
          case DictType(IntType, _: RecordType, Vec(None)) => s"$lhs.emplace_back($rhs);"
          case _                                           => s"$lhs = $rhs;"
        }

      case Timer(e) =>
        val benchStart =
          if (benchmarkRuns == 0) "\n"
          else
            // note: first benchmark run is warmup
            s"""|\n
                |HighPrecisionTimer timer;
                |for (${cppType(IntType)} iter = 0; iter <= $benchmarkRuns; iter++) {
                |timer.Reset();
                |""".stripMargin
        val benchStop  =
          if (benchmarkRuns == 0) cppPrintResult(TypeInference(e))
          else
            s"""\n
               |timer.StoreElapsedTime(0);
               |doNotOptimiseAway($resultName);
               |std::cerr << "*" << " " << std::flush;
               |}
               |std::cout << std::endl << timer.GetMean(0) << " ms";
               |""".stripMargin
        s"""$benchStart
           |${run(e)}
           |$benchStop
           |""".stripMargin

      case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
    }

  private def load(e: Load, name: String) = (e: @unchecked) match {
    case Load(path, tp: RecordType, skipCols) =>
      // note: NaN handling is required for LSQB queries - we replace NaN with the same value in every table, this works
      // a more robust approach would assign a different NaN value to each table, in case NaNs get joined - but no need
      val document    =
        s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR, IntNanConverter(0));"""
      val skipColsSet = skipCols.toSkipColsSet
      assert(tp.attrs.last.name == "size")
      val attrs       = tp.attrs
        .dropRight(1)
        .map(attr => (attr.tpe: @unchecked) match { case DictType(IntType, vt, Vec(None)) => Attribute(attr.name, vt) })
      val readCols    = attrs.zipWithIndex.filter { case (attr, _) => !skipColsSet.contains(attr.name) }.map {
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
      val readSize    =
        if (skipColsSet.contains("size")) Seq()
        else Seq(s"/* size */static_cast<${cppType(IntType)}>(${name.toUpperCase}_CSV.GetRowCount())")
      val init        = (readCols ++ readSize).mkString(",\n")
      s"""$document
         |const auto ${name.toLowerCase} = ${cppType(tp, noTemplate = true)}($init);
         |""".stripMargin
  }

  private def ternary(e: IfThenElse)(implicit typesCtx: TypesCtx, benchmarkRuns: Int)                     = e match {
    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, isTernary = true, benchmarkRuns)
      val ifBody   = run(e1)(typesCtx, isTernary = true, benchmarkRuns)
      val elseBody = run(e2)(typesCtx, isTernary = true, benchmarkRuns)
      s"($condBody) ? $ifBody : $elseBody"
  }
  private def default(e: IfThenElse)(implicit typesCtx: TypesCtx, isTernary: Boolean, benchmarkRuns: Int) = e match {
    case IfThenElse(cond, e1, e2) =>
      val condBody = run(cond)(typesCtx, isTernary, benchmarkRuns)
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

  private def dictCmpNil(e1: Exp, e2: Exp)(implicit typesCtx: TypesCtx, isTernary: Boolean, benchmarkRuns: Int) =
    TypeInference.run(e1) match {
      case DictType(IntType, _, _: Vec) => s"${run(e1)}[${run(e2)}] != 0"
      case _                            => s"${run(e1)}.contains(${run(e2)})"
    }

  private def cppLhsRhs(e: Exp, destination: Sym)(implicit typesCtx: TypesCtx, benchmarkRuns: Int)                   = {
    val (accessors, inner) = splitNested(e)
    val bracketed          = cppAccessors(accessors)(typesCtx, isTernary = true, benchmarkRuns)
    val lhs                = s"${destination.name}$bracketed"
    val rhs                = run(inner)(typesCtx, isTernary = true, benchmarkRuns)
    (lhs, rhs)
  }
  private def cppAccessors(exps: Iterable[Exp])(implicit typesCtx: TypesCtx, isTernary: Boolean, benchmarkRuns: Int) =
    exps.map(e => s"[${run(e)}]").mkString("")
  private def splitNested(e: Exp): (Seq[Exp], Exp)                                                                   =
    e match {
      case DictNode(
            Seq((k, v @ DictNode(_, _: PHmap | Range | _: SmallVecDict | _: SmallVecDicts | _: SortedDict))),
            _
          ) =>
        val (lhs, rhs) = splitNested(v)
        (Seq(k) ++ lhs, rhs)
      case DictNode(Seq((k, DictNode(Seq((rhs, Const(1))), _: Vec))), _) =>
        (Seq(k), rhs)
      case DictNode(Seq((k @ RecNode(_), Const(1))), _: Vec)             =>
        (Seq(), k)
      case DictNode(Seq((k, rhs)), _)                                    =>
        (Seq(k), rhs)
      case DictNode(map, _) if map.length != 1                           =>
        raise(s"unsupported: $e")
      case _                                                             =>
        (Seq(), e)
    }

  private def initialise(
    tpe: Type
  )(implicit agg: Aggregation, typesCtx: TypesCtx, isTernary: Boolean, benchmarkRuns: Int): String =
    tpe match {
      case DictType(_, _, PHmap(Some(e)))                                   => run(e)
      case DictType(_, _, SortedDict(Some(e)))                              => run(e)
      case DictType(_, _, PHmap(None) | SortedDict(None) | _: SmallVecDict) => "{}"
      case DictType(_, _, Vec(size))                                        =>
        size match {
          case None       => ""
          case Some(size) => (size + 1).toString
        }
      case DictType(_, _, _: SmallVecDicts)                                 =>
        ""
      case RecordType(attrs)                                                =>
        attrs.map(_.tpe).map(initialise).mkString(", ")
      case BoolType                                                         =>
        agg match {
          case SumAgg | MaxAgg  => "false"
          case ProdAgg | MinAgg => "true"
        }
      case RealType                                                         =>
        agg match {
          case SumAgg | MaxAgg => "0.0"
          case ProdAgg         => "1.0"
          case MinAgg          => s"std::numeric_limits<${cppType(RealType)}>::max()"
        }
      case IntType | DateType                                               =>
        agg match {
          case SumAgg | MaxAgg => "0"
          case ProdAgg         => "1"
          case MinAgg          => s"std::numeric_limits<${cppType(IntType)}>::max()"
        }
      case StringType(None)                                                 =>
        agg match {
          case SumAgg | MaxAgg => "\"\""
          case ProdAgg         => raise("undefined")
          case MinAgg          => s"STRING_MAX"
        }
      case StringType(Some(_))                                              =>
        raise("initialising VarChars shouldn't be needed")
      case tpe                                                              =>
        raise(s"unimplemented type: $tpe")
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
    case DictType(kt, vt, _: SortedDict)                        =>
      val template = if (noTemplate) "" else s"<${cppType(kt)}, ${cppType(vt)}>"
      s"SortedDict$template"
    case DictType(IntType, vt, _: Vec)                          =>
      val template = if (noTemplate) "" else s"<${cppType(vt)}>"
      s"std::vector$template"
    case rt: RecordType                                         =>
      val template = if (noTemplate) "" else s"<${recordParams(rt)}>"
      s"std::tuple$template"
    case DictType(_, _, Range)                                  => "Range"
    case BoolType                                               => "bool"
    case RealType                                               => "double"
    case IntType | DateType                                     => "int"
    case LongType                                               => "long"
    case StringType(None)                                       => "std::string"
    case StringType(Some(maxLen))                               => s"VarChar<$maxLen>"
    case tpe                                                    => raise(s"unimplemented type: $tpe")
  }
  private def recordParams(rt: RecordType)                            = rt match {
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType(_)).mkString(", ")
  }

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
