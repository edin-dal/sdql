package sdql
package backend

import munit.Assertions.munitPrint
import sdql.analysis.TypeInference
import sdql.ir.ExternalFunctions._
import sdql.ir._

import java.util.UUID
import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec

object CppCodegen {
  private type TypesCtx = TypeInference.Ctx
  private type CallsCtx = List[CallCtx]
  private sealed trait CallCtx
  private case class LetCtx(name: String) extends CallCtx
  private case class SumCtx(on: String, k:String, v: String, isLoad: Boolean, hint: SumCodegenHint) extends CallCtx
  private case class SumEnd() extends CallCtx
  private case class IsTernary() extends CallCtx
  private type LoadsCtx = Set[Sym]

  private val vecSize = 6000001;
  private val reDate = "^(\\d{4})(\\d{2})(\\d{2})$".r
  private val resultName = "result"
  private val noName = "_"

  private val header = """#include "../runtime/headers.h""""
  private val csvConsts =
    s"""const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
       |const auto SEPARATOR = rapidcsv::SeparatorParams('|');
       |""".stripMargin

  def apply(e: Exp, isBenchmark: Boolean = false): String = {
    val (csvBody, loadsCtx) = CsvBodyWithLoadsCtx(Seq(e))
    val queryBody = run(e)(Map(), List(), loadsCtx)
    val benchStart = if(!isBenchmark) "" else
      s"""auto start = std::chrono::high_resolution_clock::now();
        |""".stripMargin
    val benchStop = if(!isBenchmark) "" else
      """
        |auto stop = std::chrono::high_resolution_clock::now();
        |auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
        |std::cout << "Runtime (ms): " << duration.count() << std::endl;
        |""".stripMargin
    // slightly wasteful to redo type inference - but spares us having to return the type at every recursive run call
    val tpe = TypeInference(e)
    s"""$header
       |$csvConsts
       |$csvBody
       |int main() {
       |$benchStart
       |$queryBody
       |$benchStop
       |${cppPrintResult(tpe)}
       |}""".stripMargin
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx): String = {
    if (checkNoLetBindings(e)) {
      return run(LetBinding(Sym(resultName), e, DictNode(Nil)))
    }

    if (checkIsSumBody(e)) {
      val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
      val hint = callsCtx.flatMap(x => condOpt(x) { case SumCtx(_, _, _, _, hint) => hint }).head
      val callsLocal = List(SumEnd(), IsTernary()) ++ callsCtx
      return e match {
        case dict @ DictNode(seq) => seq.map(
            kv => {
              val lhs = run(kv._1)(typesCtx, callsLocal, loadsCtx)
              val rhs = run(kv._2)(typesCtx, callsLocal, loadsCtx)
              hint match {
                case _: SumNoHint => s"$agg[$lhs] += $rhs;"
                case _: SumUniqueHint => s"$agg.emplace($lhs, $rhs);"
                case _: SumVectorHint =>
                  typesCtx(Sym(agg)) match {
                    case dt @ DictType(IntType, DictType(_: RecordType, IntType, DictVectorHint()), DictNoHint()) =>
                      assert(TypeInference.isRecordToInt(dt))
                      s"$agg[$lhs].emplace_back($sumVariable);"
                    case dt: DictType if TypeInference.isRecordToInt(dt) =>
                      val cols = getRecordType(dt) match { case Some(RecordType(attrs)) => attrs.map(_.name) }
                      val exps = getRecordNode(dict) match { case RecNode(values) => values.map(_._2)}
                      val expsCpp = exps.map(run(_)(typesCtx, callsLocal, loadsCtx))
                      val insertions = cols
                        .zip(expsCpp).map { case (col, cpp) => s"${agg}_inner.$col.push_back($cpp);" }.mkString("\n")
                      val accessors = getDictKeys(dict).map {
                          case field @ FieldNode(Sym(name), _) =>
                            val fieldCpp = run(field)(typesCtx, callsLocal, loadsCtx)
                            assert(fieldCpp.startsWith(getOrigin(name)))
                            s"[$fieldCpp]"
                        }.mkString("")
                      val link = s"$agg$accessors.push_back(${agg}_cnt++);"
                      s"""$insertions
                         |$link
                         |""".stripMargin
                    case DictType(IntType, DictType(IntType, _, DictVectorHint()), DictVectorHint()) =>
                      e match {
                        case DictNode(Seq((f1: FieldNode, DictNode(Seq((f2: FieldNode, _)))))) =>
                          val lhs = run(f1)(typesCtx, callsLocal, loadsCtx)
                          val rhs = run(f2)(typesCtx, callsLocal, loadsCtx)
                          s"$agg[$lhs].emplace_back($rhs);"
                      }
                    case DictType(IntType, _, DictVectorHint()) =>
                      s"$agg[$lhs] = $rhs;"
                    case tpe =>
                      raise(s"Unexpected ${tpe.prettyPrint}")
                  }
              }
            }
          ).mkString("\n")
        case RecNode(values) =>
          values.map(_._2).zipWithIndex.map(
            { case (exp, i) => s"get<$i>($agg) += ${run(exp)(typesCtx, callsLocal, loadsCtx)};" }
          ).mkString("\n")
        case _ if cond(hint) { case _: SumMinHint => true } =>
          s"min_inplace($agg, ${run(e)(typesCtx, callsLocal, loadsCtx)});"
        case _ =>
          s"$agg += ${run(e)(typesCtx, callsLocal, loadsCtx)};"
      }
    }

    e match {
      case LetBinding(x @ Sym(name), e1, e2) =>
        val isTernary = !cond(e1) { case _: Sum => true }
        val localCalls = if (isTernary) List(IsTernary()) ++ callsCtx else callsCtx
        val e1Cpp = e1 match {
          case Load(_, DictType(RecordType(_), IntType, _)) =>
            // codegen for loads was handled in a separate tree traversal
            ""
          case External(Limit.SYMBOL, _) =>
            s"${run(e1)(typesCtx, List(LetCtx(name)) ++ localCalls, loadsCtx)}\n"

          case c: Const =>
            s"constexpr auto $name = ${const(c)};"
          case _ =>
            s"auto $name = ${run(e1)(typesCtx, List(LetCtx(name)) ++ localCalls, loadsCtx)};"
        }
        val e2Cpp = e2 match {
          case DictNode(Nil) => ""
          case _ => run(e2)(typesCtx ++ Map(x -> TypeInference.run(e1)), callsCtx, loadsCtx)
        }
        e1Cpp + e2Cpp

      case Sum(k, v, e1, e2, hint) =>
        val agg = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
        var (tpe, typesLocal) = TypeInference.sum_infer_type_and_ctx(k, v, e1, e2, hint)
        typesLocal ++= Map(Sym(agg) -> tpe)

        val isLoad = cond(e1) { case e1Sym: Sym => loadsCtx.contains(e1Sym) }
        val on = condOpt(e1) {
          case Sym(name) => name
          case Get(Sym(name), _) => name
        }.head
        val callsLocal = List(SumCtx(on, k=k.name, v=v.name, isLoad=isLoad, hint)) ++ callsCtx

        val isNestedSum = inferSumNesting(callsLocal) > 0
        val isLetSum = cond(callsCtx.head) { case _: LetCtx => true }
        val init = if (isNestedSum && !isLetSum) "" else s"${cppType(tpe)} (${cppInit(tpe)});"

        val body = run(e2)(typesLocal, callsLocal, loadsCtx)

        if (isLoad) {
          val e1Name = (e1: @unchecked) match { case Sym(e1Name) => e1Name }
          val values = if (v.name == noName) "" else s"constexpr auto ${v.name} = ${e1Name.capitalize}Values();"
          val sumVar = sumVariable(callsLocal)
          s"""$init
             |for (int $sumVar = 0; $sumVar < ${e1Name.capitalize}::size(); $sumVar++) {
             |const auto &${k.name} = $e1Name;
             |$values
             |$body
             |}
             |
             |""".stripMargin
        } else {
          val isVecSum = cond(hint) { case SumVectorHint() => true }
          val initBlock = getRecordType(tpe) match {
            case Some(RecordType(attrs)) if isVecSum && !isNestedSum =>
              val aggVectors = attrs.map(attr => s"vector<${cppType(attr.tpe)}> ${attr.name};")
                .mkString(s"struct ${agg.toUpperCase}_TYPE {", "\n", s"} ${agg}_inner;")
              s"""$init
                 |$aggVectors
                 |auto ${agg}_cnt = 0;
                 |""".stripMargin
            case _ =>
              init
          }
          val iterable = run(e1)(typesLocal, List(SumEnd()) ++ callsLocal, loadsCtx)
          val head = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, DictNoHint()) =>
              s"[${k.name}, ${v.name}] : $iterable"
            case DictType(_, _, DictVectorHint()) =>
              assert(v.name == noName)
              s"${k.name}_i : $iterable"
          }
          s"""$initBlock
             |for (const auto &$head) {
             |$body
             |}
             |""".stripMargin
        }

      // not case
      case IfThenElse(a, Const(false), Const(true)) =>
        s"!(${run(a)})"
      // and case
      case IfThenElse(a, b, Const(false)) =>
        s"(${run(a)} && ${run(b)})"
      // or case
      case IfThenElse(a, Const(true), b) =>
        s"(${run(a)} || ${run(b)})"

      case IfThenElse(cond, e1, e2) if checkIsTernary =>
        val callsLocal = List(SumEnd()) ++ callsCtx
        val condBody = run(cond)(typesCtx, callsLocal, loadsCtx)
        val ifBody = run(e1)(typesCtx, callsLocal, loadsCtx)
        val elseBody = run(e2)(typesCtx, callsLocal, loadsCtx)
          s"($condBody) ? $ifBody : $elseBody"

      case IfThenElse(cond, e1, e2) =>
        val condBody = run(cond)(typesCtx, List(SumEnd()) ++ callsCtx, loadsCtx)
        val ifBody = run(e1)
        val elseBody = e2 match {
          case DictNode(Nil) | RecNode(Seq()) | Const(0) | Const(0.0) => ""
          case _ => s" else {\n${run(e2)}\n}"
        }
        s"if ($condBody) {$ifBody\n}$elseBody"

      case Cmp(Get(e1, e2), DictNode(Nil), "!=")
        if cond(TypeInference.run(e1)) { case DictType(kt, _, _) => TypeInference.run(e2) == kt } =>
          dictCmpNil(e1, e2)
      case Cmp(DictNode(Nil), Get(e1, e2), "!=")
        if cond(TypeInference.run(e1)) { case DictType(kt, _, _) => TypeInference.run(e2) == kt } =>
          dictCmpNil(e1, e2)
      case Cmp(e1, e2, cmp) =>
        s"${run(e1)} $cmp ${run(e2)}"

      case fieldNode @ FieldNode(e1, _) =>
        TypeInference.run(e1) match {
          case recordType: RecordType => cppField(fieldNode, recordType)
          case tpe => raise(s"unexpected type: ${tpe.prettyPrint}")
        }

      case Add(e1, Neg(e2)) =>
        s"(${run(e1)} - ${run(e2)})"
      case Add(e1, e2) =>
        s"(${run(e1)} + ${run(e2)})"

      case Mult(e1, External(Inv.SYMBOL, args)) =>
        val divisor = args match { case Seq(divisorExp: Exp) => run(divisorExp) }
        s"(${run(e1)} / $divisor)"
      case Mult(e1, e2) =>
        s"(${run(e1)} * ${run(e2)})"

      case Neg(e) =>
        s"-${run(e)}"

      case c: Const =>
        const(c)

      case Sym(name) =>
        callsCtx.flatMap(x => condOpt(x) { case ctx: SumCtx => ctx }).headOption match {
          case Some(SumCtx(_, k, v, true, _)) if name == k || name == v => s"$name[$sumVariable]"
          case _ => name
        }

      case DictNode(Nil) =>
        ""
      case DictNode(seq) =>
        val localCalls = List(IsTernary()) ++ callsCtx
        seq.map( { case (e1, e2) =>
            val e1Cpp = run(e1)(typesCtx, localCalls, loadsCtx)
            val e2Cpp = run(e2)(typesCtx, localCalls, loadsCtx)
            s"{$e1Cpp, $e2Cpp}"
          })
          .mkString(s"${cppType(TypeInference.run(e))}({", ", ", "})")

      case RecNode(Seq()) =>
        ""
      case RecNode(values) =>
        val localCalls = List(IsTernary()) ++ callsCtx
        val tpe = TypeInference.run(e) match {
          case tpe: RecordType => tpe
          case tpe => raise(
            s"expression ${e.simpleName} should be of type " +
              s"${RecordType.getClass.getSimpleName.init} not ${tpe.simpleName}"
          )
        }
        values.map(e => run(e._2)(typesCtx, localCalls, loadsCtx)).mkString(s"${cppType(tpe)}(", ", ", ")")

      case Get(e1, e2) => TypeInference.run(e1) match {
        case _: RecordType =>
          s"std::get<${run(e2)}>(${run(e1)})"
        case _: DictType =>
          s"${run(e1)}[${run(e2)}]"
        case tpe => raise(
          s"expected ${RecordType.getClass.getSimpleName.init} or " +
            s"${DictType.getClass.getSimpleName.init}, not ${tpe.simpleName}"
        )
      }

      case External(ConstantString.SYMBOL, Seq(Const(str: String), Const(maxLen: Int))) =>
        assert(maxLen == str.length + 1)
        s"""ConstantString("$str", $maxLen)"""

      case External(StrContains.SYMBOL, Seq(str, subStr)) =>
        val func = (TypeInference.run(str), TypeInference.run(subStr)) match {
          case (StringType(None), StringType(None)) => "find"
          case (StringType(Some(_)), StringType(Some(_))) => "contains"
          case (StringType(None), StringType(Some(_))) | (StringType(Some(_)), StringType(None)) =>
            raise(s"${StrContains.SYMBOL} doesn't support fixed and variable length strings together")
        }
        s"${run(str)}.$func(${run(subStr)})"
      case External(StrStartsWith.SYMBOL, Seq(str, prefix)) =>
        val startsWith = TypeInference.run(str) match {
          case StringType(None) => "starts_with"
          case StringType(Some(_)) => "startsWith"
        }
        s"${run(str)}.$startsWith(${run(prefix)})"
      case External(StrEndsWith.SYMBOL, Seq(str, suffix)) =>
        val endsWith = TypeInference.run(str) match {
          case StringType(None) => "ends_with"
          case StringType(Some(_)) => "endsWith"
        }
        s"${run(str)}.$endsWith(${run(suffix)})"
      case External(SubString.SYMBOL, Seq(str, Const(start: Int), Const(end: Int))) =>
        val subStr = TypeInference.run(str) match {
          case StringType(None) => "substr"
          case StringType(Some(_)) => s"substr<${end - start}>"
        }
        s"${run(str)}.$subStr($start, $end)"
      case External(StrIndexOf.SYMBOL, Seq(field: FieldNode, elem, from)) =>
        assert(cond(TypeInference.run(field)) { case StringType(None) => true })
        s"${run(field)}.find(${run(elem)}, ${run(from)})"
      case External(FirstIndex.SYMBOL, Seq(field: FieldNode, arg)) =>
        s"${run(field)}.firstIndex(${run(arg)})"
      case External(LastIndex.SYMBOL, Seq(field: FieldNode, arg)) =>
        s"${run(field)}.lastIndex(${run(arg)})"
      case External(name @ Inv.SYMBOL, _) =>
        raise(s"$name should have been handled by ${Mult.getClass.getSimpleName.init}")
      case External(MaxValue.SYMBOL, Seq(arg)) =>
        (arg: @unchecked) match { case sym: Sym => assert(!loadsCtx.contains(sym))}
        val name = run(arg)
        s"""std::max_element($name.begin(), $name.end(), [](const auto &p1, const auto &p2) {
          |return p1.second < p2.second;
          |})->second;
          |""".stripMargin
      case External(Size.SYMBOL, Seq(arg)) =>
        condOpt(arg) { case sym @ Sym(name) if loadsCtx.contains(sym) => name } match {
          case Some(name) => s"${name.capitalize}::size()"
          case None => s"${run(arg)}.size()"
        }
      case External(Limit.SYMBOL, Seq(sym @ Sym(arg), Const(n: Int), Const(isDescending: Boolean))) =>
        val limit = callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
        val tmp = s"tmp_$uuid"
        val (dictTpe, kt, vt) =
          (TypeInference.run(sym): @unchecked) match { case dictTpe @ DictType(kt, vt, _) => (dictTpe, kt, vt) }
        val recTpe = RecordType(Seq(Attribute("_1", kt), Attribute("_2", vt)))
        val recTpeCpp = cppType(recTpe)
        val cmp = if(isDescending) ">" else "<"
        s"""std::vector<$recTpeCpp> $tmp($n);
           |std::partial_sort_copy(
           |    $arg.begin(), $arg.end(), $tmp.begin(), $tmp.end(),
           |    []($recTpeCpp const &l, $recTpeCpp const &r) { return std::get<1>(l) $cmp std::get<1>(r); });
           |auto $limit = ${cppType(dictTpe)}($tmp.begin(), $tmp.end());
           |""".stripMargin
      case External(name, _)  =>
        raise(s"unhandled function name: $name")

      case Concat(v1 @ RecNode(fs1), v2 @ RecNode(fs2)) => run(
        {
          val (fs1m, fs2m) = fs1.toMap -> fs2.toMap
          val common =
            fs1.filter(x1 => fs2m.contains(x1._1)).map(x1 => (x1._1, x1._2, fs2m(x1._1)))
          if(common.isEmpty)
            RecNode(fs1 ++ fs2)
          else
            if(common.forall(x => x._2 == x._3))
              RecNode(fs1 ++ fs2.filter(x2 => !fs1m.contains(x2._1)))
            else
              raise(s"`concat($v1, $v2)` with different values for the same field name")
        }
      )
      case Concat(e1: Sym, e2: Sym) =>
        s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1: Sym, e2: RecNode) =>
        s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1: RecNode, e2: Sym) =>
        s"std::tuple_cat(${run(e1)}, ${run(e2)})"
      case Concat(e1, e2) =>
        val _ = TypeInference.run(e)
        raise(
          s"${Concat.getClass.getSimpleName} requires arguments " +
            s"${RecNode.getClass.getSimpleName.init} and/or ${Sym.getClass.getSimpleName.init}, " +
            s"not ${e1.simpleName} and ${e2.simpleName}"
        )

      case _ => raise(
        f"""Unhandled ${e.simpleName} in
           |${munitPrint(e)}""".stripMargin
      )
    }
  }

  @tailrec
  private def getRecordType(tpe: Type): Option[RecordType] = tpe match {
    case DictType(_, dt: DictType, _) => getRecordType(dt)
    case DictType(rt: RecordType, IntType, _) => Some(rt)
    case _ => None
  }

  @tailrec
  private def getRecordNode(dict: DictNode)(implicit typesCtx: TypesCtx): RecNode = dict match {
    case DictNode(Seq((_, v: DictNode))) => getRecordNode(v)
    case DictNode(Seq((record: RecNode, Const(1)))) => record
    case _ => raise(s"unexpected: ${TypeInference.run(dict).prettyPrint}")
  }

  private def getDictKeys(dict: DictNode)(implicit typesCtx: TypesCtx): Seq[Exp] = dict match {
    case DictNode(Seq((k, v: DictNode))) => Seq(k) ++ getDictKeys(v)
    case DictNode(Seq((record: RecNode, Const(1)))) => Seq()
    case _ => raise(s"unexpected: ${TypeInference.run(dict).prettyPrint}")
  }

  private def getOrigin(name: String)(implicit callsCtx: CallsCtx) = {
    val sumOrigin = getSumOrigin(name)
    assert(sumOrigin.endsWith("_trie0"))
    sumOrigin.dropRight("_trie0".length)
  }

  @tailrec
  private def getSumOrigin(name: String)(implicit callsCtx: CallsCtx): String =
    callsCtx.flatMap(condOpt(_) { case SumCtx(origin, k, v, _, _) if name == k || name == v => origin }).headOption match {
      case Some(origin) => getSumOrigin(origin)
      case None => name
    }

  private def dictCmpNil(e1: Exp, e2: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx) = {
    val isVector = cond(TypeInference.run(e1)) { case DictType(_, _, DictVectorHint()) => true }
    if (isVector)
      s"${run(e1)}[${run(e2)}] != 0"
    else
      s"${run(e1)}.contains(${run(e2)})"
  }

  private def const(c: Const) = c match {
    case Const(DateValue(v)) =>
      val yyyymmdd = reDate.findAllIn(v.toString).matchData.next()
      s"${yyyymmdd.group(1)}${yyyymmdd.group(2)}${yyyymmdd.group(3)}"
    case Const(v: String) =>
      s""""$v""""
    case Const(v) =>
      v.toString
  }

  private def cppField(fieldNode: FieldNode, recordType: RecordType)(
    implicit typesCtx: TypesCtx, callsCtx: CallsCtx, loadsCtx: LoadsCtx) = {
    val (e1, field) = fieldNode match { case FieldNode(e1, field) => (e1, field) }
    val idx = (recordType.indexOf(field): @unchecked) match { case Some(idx) => idx }
    e1 match {
      case Sym(name)
        if callsCtx.exists(x => cond(x) { case SumCtx(_, k, v, true, _) => name == k || name == v}) =>
        s"$name.$field[${sumVariable(name)}]"
      // TODO get rid of hack for job/gj queries
      case Sym(name)
        if name.endsWith("_tuple") && !callsCtx.exists(cond(_) { case SumCtx(_, _, _, _, SumMinHint()) => true }) =>
        s"${name.dropRight("_tuple".length)}.$field[${name}_i]"
      case Sym(name) if callsCtx.exists(cond(_) { case SumCtx(_, _, _, _, SumMinHint()) => true }) =>
        val origin = getOrigin(name)
        s"${origin}_trie0_inner.$field[${origin}_tuple_i]"
      case _ =>
        s" /* $field */ std::get<$idx>(${run(e1)})"
    }
  }

  private def cppInit(tpe: ir.Type): String = tpe match {
    case BoolType => "false"
    case RealType => "0.0"
    case IntType | DateType => "0"
    case StringType(None) => ""
    case StringType(Some(_)) => raise("initialising VarChars shouldn't be needed")
    case DictType(_, _, DictNoHint()) => "{}"
    case DictType(_, _, DictVectorHint()) => vecSize.toString
    case RecordType(attrs) => attrs.map(_.tpe).map(cppInit).mkString(", ")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def cppType(tpe: ir.Type): String = tpe match {
    case BoolType => "bool"
    case RealType => "double"
    case IntType | DateType => "long"
    case StringType(None) => "std::string"
    case StringType(Some(maxLen)) => s"VarChar<$maxLen>"
    case DictType(kt, vt, DictNoHint()) => s"phmap::flat_hash_map<${cppType(kt)}, ${cppType(vt)}>"
    case DictType(IntType | _: RecordType, vt, DictVectorHint()) => s"vector<${cppType(vt)}>"
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType).mkString("std::tuple<", ", ", ">")
    case tpe => raise(s"unimplemented type: $tpe")
  }

  private def CsvBodyWithLoadsCtx(exps: Seq[Exp]): (String, LoadsCtx) = {
    val pathNameAttrs = exps.flatMap( e =>
      iterExps(e)
        .flatMap(
          e => condOpt(e) {
            case LetBinding(Sym(name), Load(path, DictType(recordType: RecordType, IntType, _)), _) =>
              (path, name, recordType)
          }
        )
    ).toSet

    val csvConsts =
      pathNameAttrs.map({ case (path, name, _) => makeCsvConst(name, path) } ).mkString("", "\n", "\n")
    val structDefs =
      pathNameAttrs.map({ case (_, name, recordType) => makeStructDef(name, recordType) } ).mkString("\n")
    val structInits =
      pathNameAttrs.map({ case (_, name, recordType) => makeStructInit(name, recordType) } ).mkString("\n")
    val valueClasses =
      pathNameAttrs.map({ case (_, name, _) => makeValuesClass(name) } ).mkString("\n")

    (
      List(csvConsts, structDefs, structInits, valueClasses).mkString("\n"),
      pathNameAttrs.map(_._2).map(Sym.apply),
    )
  }

  private def makeCsvConst(name: String, path: String): String =
    s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR);"""

  private def makeStructDef(name: String, recordType: RecordType): String = {
    val cppFields = recordType.attrs.map(attr => s"std::vector<${cppType(attr.tpe)}> ${attr.name};").mkString("\n")
    val cppSize = s"static unsigned long size() { return ${name.toUpperCase}_CSV.GetRowCount(); }"
    val cppTuple = recordType.attrs.map(_.name).map(name => s"$name[i]").mkString("{", ", ", "}")
    val cppBrackets = s"${cppType(recordType)} operator[](const int i) const { return $cppTuple; }"
    s"""struct ${name.capitalize} {
       |$cppFields
       |$cppSize
       |$cppBrackets
       |};
       |""".stripMargin
  }

  private def makeStructInit(name: String, recordType: RecordType): String =
    recordType.attrs.zipWithIndex.map(
        {
          case (Attribute(_, tpe), i) => tpe match {
            case StringType(Some(maxLen)) =>
              val tpe = StringType()
              s"strings_to_varchars<$maxLen>(" +
                s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i)" +
                "),"
            case _ =>
              s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i),"
          }
        }
      )
      .mkString(s"const ${name.capitalize} ${name.toLowerCase} {\n", "\n", "\n};\n")

  private def makeValuesClass(name: String): String =
    s"""
       |class ${name.capitalize}Values{
       |public:
       |int operator[](const int i) const { return 0 <= i < ${name.toUpperCase}_CSV.GetRowCount(); }
       |};
       |""".stripMargin

  private def iterExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: RangeNode | _: Load =>
          Iterator()
        // 1-ary
        case Neg(e) =>
          iterExps(e)
        case FieldNode(e, _) =>
          iterExps(e)
        case Promote(_, e) =>
          iterExps(e)
        // 2-ary
        case Add(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Mult(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Cmp(e1, e2, _) =>
          iterExps(e1) ++ iterExps(e2)
        case Sum(_, _, e1, e2, _) =>
          iterExps(e1) ++ iterExps(e2)
        case Get(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case Concat(e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        case LetBinding(_, e1, e2) =>
          iterExps(e1) ++ iterExps(e2)
        // 3-ary
        case IfThenElse(e1, e2, e3) =>
          iterExps(e1) ++ iterExps(e2) ++ iterExps(e3)
        // n-ary
        case RecNode(values) =>
          values.map(_._2).flatMap(iterExps).toList
        case DictNode(dict) =>
          dict.flatMap(x => iterExps(x._1) ++ iterExps(x._2)).toList
        case External(_, args) =>
          args.flatMap(iterExps).toList
        // unhandled
        case _ => raise(
          f"""Unhandled ${e.simpleName} in
             |${munitPrint(e)}""".stripMargin
        )
      }
    )

  private def checkNoLetBindings(e: Exp)(implicit callsCtx: CallsCtx) =
    !cond(e) { case _: LetBinding => true } && !callsCtx.exists(cond(_) { case _: LetCtx  => true })

  private def checkIsSumBody(e: Exp)(implicit callsCtx: CallsCtx): Boolean = {
    !cond(e) { case _: LetBinding | _: IfThenElse | _: Sum => true } &&  checkActiveSumCtx
  }

  private def checkActiveSumCtx(implicit callsCtx: CallsCtx) = {
    callsCtx.indexWhere(x => cond(x) { case _: SumCtx => true }) match {
      case -1 => false
      case start => callsCtx.indexWhere(x => cond(x) { case _: LetCtx |  _: SumEnd => true }) match {
        case -1 => true
        case end =>
          start < end
      }
    }
  }

  private def checkIsTernary(implicit callsCtx: CallsCtx) =
    callsCtx.exists(cond(_) { case _: IsTernary  => true })

  private def sumVariable(implicit callsCtx: CallsCtx) = ('i'.toInt + inferSumNesting).toChar

  private def inferSumNesting(implicit callsCtx: CallsCtx) =
    (callsCtx.takeWhile(!cond(_) { case _: SumEnd => true }).count(cond(_) { case _: SumCtx => true }) - 1).max(0)

  private def sumVariable(name: String)(implicit callsCtx: CallsCtx) = {
    val lvlMax = (callsCtx.count(cond(_) { case _: SumCtx => true }) - 1).max(0)
    val lvl =
      callsCtx.takeWhile(!cond(_) { case SumCtx(_, k, _, _, _) => k == name }).count(cond(_) { case _: SumCtx => true })
    ('i'.toInt + (lvlMax - lvl)).toChar
  }

  private def cppPrintResult(tpe: Type) = tpe match {
    case DictType(kt, vt, DictNoHint()) =>
      s"""for (const auto &[key, val] : $resultName) {
         |$stdCout << ${_cppPrintResult(kt, "key")} << ":" << ${_cppPrintResult(vt, "val")} << std::endl;
         |}""".stripMargin
    case DictType(kt, vt, DictVectorHint()) =>
      assert(kt == IntType, s"${kt.simpleName} != ${IntType.simpleName}")
      val cond = vt match {
        case _: DictType =>
          s"!$resultName[i].empty()"
        case _: RecordType =>
          raise(s"Print not implemented for ${vt.simpleName} inside vector")
        case t =>
          assert(t.isScalar)
          s"$resultName[i] != 0"
      }
      s"""for (auto i = 0; i < $resultName.size(); ++i) {
         |if ($cond) {
         |$stdCout << ${_cppPrintResult(kt, "i")} << ":" << ${_cppPrintResult(vt, s"$resultName[i]")} << std::endl;
         |}
         |}""".stripMargin
    case _ =>
      s"$stdCout << ${_cppPrintResult(tpe, resultName)} << std::endl;"
  }

  private def _cppPrintResult(tpe: Type, name: String) = tpe match {
    case _: DictType =>
      // we currently don't pretty print inside nested dicts
      name
    case RecordType(attrs) => attrs.zipWithIndex.map(
      {
        case (Attribute(_, tpe: RecordType), _) =>
          raise(s"Nested ${tpe.simpleName} not supported")
        case (Attribute(_, DateType), i) =>
          s"print_date(std::get<$i>($name))"
        case (Attribute(_, _), i) =>
          s"std::get<$i>($name)"
      }
    ).mkString(""""<" <<""", """ << "," << """, """<< ">"""")
    case _ =>
      assert(tpe.isScalar)
      name
  }

  private val stdCout = s"std::cout << std::setprecision (std::numeric_limits<double>::digits10)"

  private def uuid = UUID.randomUUID.toString.replace("-", "_")
}
