package sdql
package backend

import sdql.analysis.TypeInference
import sdql.ir._

import java.util.UUID
import scala.collection.mutable.ArrayBuffer


object CppCodeGenerator {
	private type TypesCtx = TypeInference.Ctx
	private type CallsCtx = List[CallCtx]

	private sealed trait CallCtx

	private case class SumCtx(key: String, value: String, itVar: String, resVar: String) extends CallCtx

	private def defaultResultsVar = "res"

	def apply(e: Exp): String = {
		val (loadBody, mainBody, Some((Sym(name), tpe))) = run(e)(Map(), List(), defaultResultsVar)
		val printBody = tpe match {
			case _: RecordType => s"$name.print();"
		}
		s"""|#include "../../runtime/headers.h"
			|#include <limits>
			|#include <vector>
			|
			|using namespace std;
			|
			|const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
			|const auto SEPARATOR = rapidcsv::SeparatorParams('|');
			|
			|class Values { public: int operator [](int _) const { return 1; } };
			|
			|int main() {
			|${resInit2Cpp(name, tpe)}
			|
			|HighPrecisionTimer timer;
			|
			|$loadBody
			|cout << timer.GetElapsedTime() / 1000.0 << " s" << endl;
			|
			|for (int iter = 0; iter < 1 + 5; ++iter) {
			|$name.clear();
			|timer.Reset();
			|$mainBody
			|timer.StoreElapsedTime(0);
			|cerr << "*" << " " << flush;
			|}
			|cerr << endl;
			|
			|$printBody
			|cout << timer.GetMean(0) << " ms" << endl;
			|}""".stripMargin
	}

	def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, String, Option[(Sym, Type)]) = e match {
		case LetBinding(_, _: Load, _) =>
			val (loadBody, e1, loadTypesCtx) = runLoad(e)
			val (mainBody, info) = runMain(e1)(loadTypesCtx, callsCtx, resVar)
			(loadBody, mainBody, info)
	}

	private def runLoad(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, Exp, TypesCtx) = e match {
		case LetBinding(x@Sym(varName), e1@Load(path, tp), e2) =>
			val e1Cpp = load2Cpp(varName, path, tp)
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val (e2Cpp, e3, typesCtxInner) = runLoad(e2)(typesLocal, callsCtx, resVar)
			(e1Cpp + e2Cpp, e3, typesCtxInner)
		case _ => ("", e, typesCtx)
	}

	private def runMain(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, Option[(Sym, Type)]) = e match {
		case LetBinding(x@Sym(varName), e1: Sum, e2) =>
			val e1Cpp = letSum(e1)(typesCtx, callsCtx, varName)
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val (e2Cpp, info) = runMain(e2)(typesLocal, callsCtx, resVar)
			(e1Cpp + e2Cpp, info)

		case e: Sum => sum(e)
	}

	private def sum(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, Option[(Sym, Type)]) = {
		val (key, value, itVar, sumBody) = e match {
			case Sum(key: Sym, value: Sym, itVar: Sym, body: Exp) => (key.name, value.name, itVar.name, body)
		}
		var (resType, typesLocal) = TypeInference.sum_with_ctx(e)
		typesLocal ++= Map(Sym(resVar) -> resType)

		val callsLocal = List(SumCtx(key = key, value = value, itVar = itVar, resVar = resVar)) ++ callsCtx

		(loop2Cpp(key, value, itVar, sumBody)(typesLocal, callsLocal, resVar), Some(Sym(resVar), resType))
	}

	private def letSum(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = {
		val (sumCpp, Some((_, tpe))) = sum(e)

		val itVar = e.e1 match {
			case Sym(name) => Sym(name)
		}
		val structDef = typesCtx.get(itVar) match {
			case Some(_: DictType) => struct2Cpp(resVar, tpe)
			case Some(_) => ""
		}

		s"""$structDef
		   |${resInit2Cpp(resVar, tpe)}
		   |$sumCpp
		   |""".stripMargin
	}

	private def load2Cpp(varName: String, path: String, tp: Type): String = {
		tp match {
			case DictType(RecordType(fs), IntType) =>
				val doc_def = s"""const rapidcsv::Document ${docVar(varName)}("$path", NO_HEADERS, SEPARATOR);"""
				val struct_def = fs.map(
					attr => s"vector<${type2Cpp(attr.tpe)}> ${attr.name};"
				).mkString(s"struct ${typeVar(varName)} {\n", "\n", "\n};")
				val struct_init = fs.zipWithIndex.map(
					{ case (attr, i) => s"${docVar(varName)}.GetColumn<${type2Cpp(attr.tpe)}>($i)," }
				).mkString(s"const ${typeVar(varName)} $varName {\n", "\n", "\n};")
				s"""$doc_def
				   |$struct_def
				   |$struct_init
				   |""".stripMargin
		}
	}

	private def loop2Cpp(canLoopKey: String, canLoopVal: String, itVar: String, sumBody: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = {
		val loopKey = if (canLoopKey == "_") randomVar else canLoopKey
		val loopVal = if (canLoopVal == "_") randomVar else canLoopVal

		val (loopHeader, loopBody) = typesCtx.get(Sym(itVar)) match {
			case Some(RelationType(_)) =>
				(
					s"""const ${typeVar(itVar)} &$loopKey = $itVar;
					   |constexpr auto $loopVal = Values();
					   |for (int i = 0; i < ${docVar(itVar)}.GetRowCount(); ++i)""".stripMargin,
					scope2Cpp(sumBody)
				)
			case Some(DictType(IntType, _)) => (s"for (const auto &[$loopKey, $loopVal] : $itVar)", scope2Cpp(sumBody))
			case Some(DictType(_: RecordType, _)) => (s"for (const auto &${loopKey}_off : $itVar)", scope2Cpp(sumBody, isOffAvail = true))
		}

		s"""$loopHeader {
		   |$loopBody
		   |}
		   |""".stripMargin
	}

	private def scope2Cpp(body: Exp, isOffAvail: Boolean = false)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = body match {
		case LetBinding(x@Sym(varName), e1, e2) =>
			val cpp_e1 = e1 match {
				case e1: Sum => letSum(e1)(typesCtx, callsCtx, varName)
				case Get(Sym(trieVar), Sym(xVar)) => s"const auto &$varName = $trieVar.at($xVar);\n"
			}
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val cpp_e2 = scope2Cpp(e2)(typesLocal, callsCtx, resVar)
			cpp_e1 + cpp_e2
		case IfThenElse(Cmp(Sym(xVar), Sym(trieVar), "∈"), thenp, _) => s"if ($trieVar.contains($xVar)) {\n${scope2Cpp(thenp)}\n}\n"
		case _: Sum => sum(body)._1
		case _ =>
			val (assignParams, preLines) = assign2Cpp(body, isOffAvail)
			s"""$preLines
			   |$resVar$assignParams;
			   |""".stripMargin
	}

	private def assign2Cpp(body: Exp, isOffAvail: Boolean)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, String) = body match {
		case Const(v) =>
			val valueCpp = v match {
				case v: String => s""""$v""""
				case _ => v.toString
			}
			(s"+= $valueCpp", "")
		case FieldNode(Sym(tupleVar), column) =>
			val idx = if (isOffAvail) s"${tupleVar}_off" else "i"
			(s"+= $tupleVar.$column[$idx]", "")
		case DictNode(ArrayBuffer((FieldNode(Sym(tupleVar), column), valueNode))) =>
			val idx = if (isOffAvail) s"${tupleVar}_off" else "i"
			val (rhs, preLines) = assign2Cpp(valueNode, isOffAvail)
			(s"[$tupleVar.$column[$idx]]$rhs", preLines)
		case DictNode(Nil) => ("", "")
		case DictNode(ArrayBuffer((_: Sym, _: Const))) => (".push_back(i)", "")
		case DictNode(ArrayBuffer((RecNode(tpl: Seq[(Field, FieldNode)]), _: Const))) =>
			val intermVar = resVar.slice(0, 7)
			(
				s".push_back(${intermVar}_cnt++)",
				tpl.map { case (intermCol, FieldNode(Sym(relTuple), relCol: Field)) => s"$intermVar.$intermCol.push_back($relTuple.$relCol[${relTuple}_off]);" }.mkString("\n")
			)
		case RecNode(fs) =>
			(
				fs.map { case (_, FieldNode(Sym(tupleVar), column)) =>
					val subs = if (isOffAvail) s"[${tupleVar}_off]" else ""
					s"$tupleVar.$column$subs"
				}.mkString(".min(", ", ", ")"),
				""
			)
	}

	private def resInit2Cpp(varName: String, tpe: Type): String = tpe match {
		case IntType => s"${type2Cpp(tpe)} $varName = 0;"
		case StringType => s"""${type2Cpp(tpe)} $varName = "";"""
		case DictType(_, _) => s"${type2Cpp(tpe)} $varName({});"
		case RecordType(fs) =>
			s"""struct ${typeVar(varName)} {
			   |${fs.map(attr => s"${type2Cpp(attr.tpe)} ${attr.name};").mkString("\n")}
			   |${typeVar(varName)}() : ${fs.map(attr => s"${attr.name}(${inf2Cpp(attr.tpe)})").mkString(", ")} {}
			   |${typeVar(varName)}(${fs.map(attr => s"${type2Cpp(attr.tpe)} ${attr.name}").mkString(", ")}) : ${fs.map(attr => s"${attr.name}(${attr.name})").mkString(", ")} {}
			   |inline void min(${fs.map(attr => s"${type2Cpp(attr.tpe)} other_${attr.name}").mkString(", ")}) {
			   |${fs.map(attr => s"${attr.name} = std::min(${attr.name}, other_${attr.name});").mkString("\n")}
			   |}
			   |inline void print() {
			   |${fs.map(_.name).mkString("cout << ", """ << " | " << """, " << endl;")}
			   |}
			   |inline void clear() {
			   |${fs.map(attr => s"${attr.name} = ${inf2Cpp(attr.tpe)};").mkString("\n")}
			   |}
			   |} $varName;
			   |""".stripMargin
	}

	private def struct2Cpp(varName: String, tp: Type): String = tp match {
		case DictType(IntType, value) => struct2Cpp(varName, value)
		case DictType(RecordType(fs), IntType) =>
			val intermVar = varName.dropRight(6)
			val vecs = fs.map(attr => s"vector<${type2Cpp(attr.tpe)}> ${attr.name};").mkString("\n")
			s"""struct ${typeVar(intermVar)} { $vecs } $intermVar;
			   |auto &${intermVar}_tuple = $intermVar;
			   |int ${intermVar}_cnt = 0;""".stripMargin
		case _ => ""
	}

	private def type2Cpp(tpe: Type): String = tpe match {
		case IntType => "int"
		case StringType => "string"
		case DictType(_: RecordType, IntType) => "vector<int>"
		case DictType(key, value) => s"phmap::flat_hash_map<${type2Cpp(key)}, ${type2Cpp(value)}>"
	}

	private def inf2Cpp(tp: Type): String = tp match {
		case IntType => "INT_MAX"
		case StringType => """"zzzzzzzzzzzzzzzzzz""""
	}

	private def docVar(name: String) = s"${name.toUpperCase}_DOC"

	private def typeVar(name: String) = s"${name.toUpperCase}_TYPE"

	private def randomVar = s"v_${UUID.randomUUID.toString.replace("-", "_")}"
}
