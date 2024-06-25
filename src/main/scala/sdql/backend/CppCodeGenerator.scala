package sdql
package backend

import sdql.analysis.TypeInference
import sdql.ir._

import java.util.UUID
import scala.PartialFunction.condOpt
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
			|const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
			|const auto SEPARATOR = rapidcsv::SeparatorParams('|');
			|
			|class Values { public: int operator [](int _) const { return 1; } };
			|
			|int main() {
			|${resVarInit(name, tpe)}
			|
			|$loadBody
			|
			|HighPrecisionTimer timer;
			|for (int iter = 0; iter < 1 + 5; ++iter) {
			|$name.clear();
			|timer.Reset();
			|$mainBody
			|timer.StoreElapsedTime(0);
			|}
			|
			|$printBody
			|std::cout << timer.GetMean(0) << " ms" << std::endl;
			|}""".stripMargin
	}

	def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, String, Option[(Sym, Type)]) = e match {
		case LetBinding(_, _: Load, _) =>
			val (loadBody, e1, loadTypesCtx) = runLoad(e)
			val (mainBody, info) = runMain(e1)(loadTypesCtx, callsCtx, resVar)
			(loadBody, mainBody, info)
	}

	def runLoad(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, Exp, TypesCtx) = e match {
		case LetBinding(x@Sym(varName), e1@Load(path, tp), e2) =>
			val load_e1 = load(varName, path, tp)
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val (load_e2, e3, typesCtxInner) = runLoad(e2)(typesLocal, callsCtx, resVar)
			(load_e1 + load_e2, e3, typesCtxInner)
		case _ => ("", e, typesCtx)
	}

	def srunMain(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = runMain(e)._1

	def runMain(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, Option[(Sym, Type)]) = e match {
		case LetBinding(x@Sym(varName), e1, e2) =>
			val cpp_e1 = e1 match {
				case e1: Sum => letSum(e1)(typesCtx, callsCtx, varName)
			}
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val (cpp_e2, info) = runMain(e2)(typesLocal, callsCtx, resVar)
			(cpp_e1 + cpp_e2, info)

		case e: Sum => sum(e)

		case Cmp(e1, e2, cmp) => cmp match {
			case "∈" => (s"${srunMain(e2)}.contains(${srunMain(e1)})", None)
		}

		case DictNode(Nil) => ("", None)
		case DictNode(ArrayBuffer((_, htValue))) => (srunMain(htValue), None)

		case Get(Sym(htName), Sym(htKeyName)) => (s"$htName.at($htKeyName)", None)

		case Sym(name) =>
			val itVar = callsCtx.flatMap(x => condOpt(x) { case SumCtx(_, _, itVar, _) => itVar }).iterator.next()
			typesCtx.get(Sym(itVar)) match {
				case Some(_: RelationType) => (s"$name[i]", None)
				case Some(_) => (name, None)
			}

		case Const(v) => v match {
			case v: String => (s""""$v"""", None)
			case _ => (v.toString, None)
		}
	}

	private def load(varName: String, path: String, tp: Type): String = {
		val doc_def = s"""const rapidcsv::Document ${docVar(varName)}("$path", NO_HEADERS, SEPARATOR);\n"""
		tp match {
			case DictType(RecordType(fs), IntType) =>
				val struct_def = fs.map(
					attr => s"std::vector<${toCpp(attr.tpe)}> ${attr.name};"
				).mkString(s"struct ${typeVar(varName)} {\n", "\n", "\n};\n")
				val struct_init = fs.zipWithIndex.map(
					{ case (attr, i) => s"${docVar(varName)}.GetColumn<${toCpp(attr.tpe)}>($i)," }
				).mkString(s"const ${typeVar(varName)} $varName {\n", "\n", "\n};\n")
				s"$doc_def\n$struct_def\n$struct_init\n".stripMargin
		}
	}

	private def letSum(e: Sum)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = {
		val itVar = e.e1 match {
			case Sym(name) => name
		}

		val (sum_cpp, Some((_, tpe))) = sum(e)

		val struct = typesCtx.get(Sym(itVar)) match {
			case Some(_: DictType) => intermStruct(resVar, tpe)
			case Some(_) => ""
		}

		s"""$struct
		   |${resVarInit(resVar, tpe)}
		   |$sum_cpp
		   |""".stripMargin
	}

	private def sum(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, Option[(Sym, Type)]) = {
		val (key, value, itVar, sumBody) = e match {
			case Sum(key: Sym, value: Sym, itVar: Sym, body: Exp) => (key.name, value.name, itVar.name, body)
		}
		var (resType, typesLocal) = TypeInference.sum_with_ctx(e)
		typesLocal ++= Map(Sym(resVar) -> resType)

		val callsLocal = List(SumCtx(key = key, value = value, itVar = itVar, resVar = resVar)) ++ callsCtx

		(
			loop(key, value, itVar, sumBody)(typesLocal, callsLocal, resVar),
			Some(Sym(resVar), resType)
		)
	}

	private def loop(canLoopKey: String, canLoopVal: String, itVar: String, sumBody: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = {
		val loopKey = if (canLoopKey == "_") randomVar else canLoopKey
		val loopVal = if (canLoopVal == "_") randomVar else canLoopVal

		val (loopHeader, loopBody) = typesCtx.get(Sym(itVar)) match {
			case Some(RelationType(_)) =>
				(
					s"""const ${typeVar(itVar)} &$loopKey = $itVar;
					   |constexpr auto $loopVal = Values();
					   |for (int i = 0; i < ${docVar(itVar)}.GetRowCount(); ++i)""".stripMargin,
					scopeBody(sumBody, isTrieBody = true)
				)
			case Some(DictType(IntType, _)) => (s"for (const auto &[$loopKey, $loopVal] : $itVar)", scopeBody(sumBody))
			case Some(DictType(_: RecordType, _)) => (s"for (const auto &${loopKey}_off : $itVar)", scopeBody(sumBody))
		}

		s"""$loopHeader {
		   |$loopBody
		   |}
		   |""".stripMargin
	}

	private def scopeBody(body: Exp, isTrieBody: Boolean = false)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): String = body match {
		case LetBinding(x@Sym(varName), e1, e2) =>
			val cpp_e1 = e1 match {
				case e1: Sum => letSum(e1)(typesCtx, callsCtx, varName)
				case _: Get => s"const auto $varName = ${srunMain(e1)};\n"
			}
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val cpp_e2 = scopeBody(e2)(typesLocal, callsCtx, resVar)
			cpp_e1 + cpp_e2
		case IfThenElse(cond, thenp, _) => s"if (${srunMain(cond)}) {\n${scopeBody(thenp)}\n}\n"
		case _: Sum => sum(body)._1
		case _ =>
			val (assignParams, preLines) = dictAssign(body, isTrieBody)
			assignParams match {
				case "" => ""
				case _ =>
					s"""$preLines
					   |$resVar$assignParams;""".stripMargin
			}
	}

	private def dictAssign(body: Exp, isTrieBody: Boolean)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx, resVar: String): (String, String) = body match {
		case _: Const => (s"+= ${srunMain(body)}", "")
		case FieldNode(Sym(tupleVar), column) =>
			val idx = if (isTrieBody) "i" else s"${tupleVar}_off"
			(s"+= $tupleVar.$column[$idx]", "")
		case DictNode(ArrayBuffer((FieldNode(Sym(tupleVar), column), valueNode))) =>
			val idx = if (isTrieBody) "i" else s"${tupleVar}_off"
			val (rhs, preLines) = dictAssign(valueNode, isTrieBody)
			(s"[$tupleVar.$column[$idx]]$rhs", preLines)
		case DictNode(Nil) => ("", "")
		case DictNode(ArrayBuffer((_: Sym, _: Const))) => (".push_back(i)", "")
		case DictNode(ArrayBuffer((RecNode(tpl), _: Const))) =>
			val intermVar = resVar.slice(0, 7)
			(s".push_back(${intermVar}_cnt++)", tpl.map(intermColPushBack).map(col => s"$intermVar.$col").mkString("\n"))
		case RecNode(fs) => (s".min(${fs.map(x => srunMain(x._2)).mkString(", ")})", "")
	}

	private def intermColPushBack(inp: (Field, Exp)): String = {
		val (intermCol, FieldNode(Sym(relTuple), col)) = inp
		s"$intermCol.push_back($relTuple.$col[${relTuple}_off]);"
	}

	private def intermStruct(varName: String, tp: Type): String = tp match {
		case DictType(IntType, value) => intermStruct(varName, value)
		case DictType(RecordType(fs), IntType) =>
			val intermVar = varName.slice(0, 7)
			val vecs = fs.map(attr => s"std::vector<${toCpp(attr.tpe)}> ${attr.name};").mkString("\n")
			s"""struct ${typeVar(intermVar)} { $vecs } $intermVar;
			   |auto &${intermVar}_tuple = $intermVar;
			   |int ${intermVar}_cnt = 0;""".stripMargin
		case StringType | IntType | RealType => ""
	}

	private def resVarInit(varName: String, tpe: Type): String = tpe match {
		case DictType(_, _) => s"${toCpp(tpe)} $varName({});"
		case IntType => s"${toCpp(tpe)} $varName = 0;"
		case StringType => s"""${toCpp(tpe)} $varName = "";"""
		case RecordType(fs) =>
			s"""struct ${typeVar(varName)} {
			   |${fs.map(attr => s"${toCpp(attr.tpe)} ${attr.name};").mkString("\n")}
			   |${typeVar(varName)}() : ${fs.map(attr => s"${attr.name}(${inf(attr.tpe)})").mkString(", ")} {}
			   |${typeVar(varName)}(${fs.map(attr => s"${toCpp(attr.tpe)} ${attr.name}").mkString(", ")}) : ${fs.map(attr => s"${attr.name}(${attr.name})").mkString(", ")} {}
			   |void min(${fs.map(attr => s"${toCpp(attr.tpe)} other_${attr.name}").mkString(", ")}) {
			   |${fs.map(attr => s"${attr.name} = std::min(${attr.name}, other_${attr.name});").mkString("\n")}
			   |}
			   |void print() {
			   |${fs.map(_.name).mkString("std::cout << ", """ << " | " << """, " << std::endl;")}
			   |}
			   |void clear() {
			   |${fs.map(attr => s"${attr.name} = ${inf(attr.tpe)};").mkString("\n")}
			   |}
			   |} $varName;
			   |""".stripMargin
	}

	private def toCpp(tpe: Type): String = tpe match {
		case IntType => "int"
		case StringType => "std::string"
		case DictType(_: RecordType, IntType) => "std::vector<int>"
		case DictType(key, value) => s"phmap::flat_hash_map<${toCpp(key)}, ${toCpp(value)}>"
	}

	private def inf(tp: Type): String = tp match {
		case IntType => "INT_MAX"
		case StringType => """"zzzzzzzzzzzzzzzzzz""""
	}

	private def docVar(name: String) = s"${name.toUpperCase}_DOC"

	private def typeVar(name: String) = s"${name.toUpperCase}_TYPE"

	private def randomVar = s"v_${UUID.randomUUID.toString.replace("-", "_")}"
}
