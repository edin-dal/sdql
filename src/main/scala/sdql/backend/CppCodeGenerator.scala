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

	def apply(e: Exp): String = {
		val (mainBody, Some((Sym(name), tpe))) = run(e)(Map(), List())
		val printBody = tpe match {
			case _: DictType => s"std::cout << res.size() << std::endl;"
			case _ if tpe.isScalar => s"std::cout << $name << std::endl;"
		}
		s"""|#include "../runtime/headers.h"
			|#include <vector>
			|
			|const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
			|const auto SEPARATOR = rapidcsv::SeparatorParams('|');
			|
			|class Values { public: int operator [](int _) const { return 1; } };
			|
			|int main() {
			|auto res = ${toCpp(tpe)}();
			|
			|$mainBody
			|
			|$printBody
			|}""".stripMargin
	}

	def srun(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = run(e)._1

	def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): (String, Option[(Sym, Type)]) = e match {
		case LetBinding(x@Sym(varName), e1, e2) =>
			val cpp_e1 = e1 match {
				case Load(path, tp) => load(varName, path, tp)
				case e1: Sum =>
					s"auto $varName = ${sum(e1, Some(varName))._1}"
				case _: Get => s"const auto $varName = ${srun(e1)};\n"
				case _ => raise(s"Unhandled bind value $e1")
			}
			val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
			val (cpp_e2, info) = run(e2)(typesLocal, callsCtx)
			(cpp_e1 + cpp_e2, info)

		case e: Sum => sum(e)

		case IfThenElse(cond, thenp, elsep) =>
			val cppCond = srun(cond)
			val cppThen = scopeBody(thenp)
			val cppElse = scopeBody(elsep)
			(s"if ($cppCond) {\n$cppThen\n} else {\n$cppElse\n}\n", None)

		case Cmp(e1, e2, cmp) => cmp match {
			case "∈" => (s"${srun(e2)}.contains(${srun(e1)})", None)
			case _ => raise(s"not supported cmp: $e")
		}

		case DictNode(Nil) => ("", None)
		case DictNode(ArrayBuffer((_, htValue))) => (srun(htValue), None)

		case FieldNode(Sym(name), column) => (s"$name.$column[i]", None)

		case Get(Sym(htName), Sym(htKeyName)) => (s"$htName.at($htKeyName)", None)

		case Sym(name) =>
			val itVar = callsCtx.flatMap(x => condOpt(x) { case SumCtx(_, _, itVar, _) => itVar }).iterator.next()
			typesCtx.get(Sym(itVar)) match {
				case Some(_: RelationType) => (s"$name[i]", None)
				case Some(_) => (name, None)
				case None => raise(s"itVar of $name doesn't exist")
			}

		case Const(v) => v match {
			case v: String => (s""""$v"""", None)
			case _ => (v.toString, None)
		}

		case _ => raise(s"not supported in run: $e")
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
			case _ => raise(s"`load[$tp]('$path')` only supports the type `{ < ... > -> int }`")
		}
	}

	private def sum(e: Sum, canResVarName: Option[String] = None)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): (String, Option[(Sym, Type)]) = {
		val (key, value, itVar, sumBody) = e match {
			case Sum(key: Sym, value: Sym, itVar: Sym, body: Exp) => (key.name, value.name, itVar.name, body)
			case _ => raise(s"not supported sum: $e")
		}
		var (resType, typesLocal) = TypeInference.sum_with_ctx(e)
		val resVar = canResVarName match {
			case None => s"res"
			case Some(varName) => varName
		}
		typesLocal ++= Map(Sym(resVar) -> resType)

		val zeroValue = resType match {
			case RealType | IntType => "0"
			case DictType(_, _) => ""
			case _ => raise(s"unhandled type: $resType")
		}
		val resVarInit = canResVarName match {
			case Some(_) => s"${toCpp(resType)} ($zeroValue);"
			case None => ""
		}

		val callsLocal = List(SumCtx(key = key, value = value, itVar = itVar, resVar = resVar)) ++ callsCtx

		(
			s"""$resVarInit
			   |${loop(key, value, itVar, sumBody)(typesLocal, callsLocal)}
			   |""".stripMargin,
			Some(Sym(resVar), resType)
		)
	}

	private def loop(canLoopKey: String, canLoopVal: String, itVar: String, sumBody: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
		val loopKey = if (canLoopKey == "_") randomVar() else canLoopKey
		val loopVal = if (canLoopVal == "_") randomVar() else canLoopVal

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
			case Some(tpe) => raise(s"Unhandled type for iteration $tpe")
			case None => raise(s"$itVar doesn't exist in typesCtx")
		}

		s"""$loopHeader {
		   |$loopBody
		   |}
		   |""".stripMargin
	}

	private def scopeBody(body: Exp, isTrieBody: Boolean = false)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = body match {
		case _: LetBinding => srun(body)
		case _: IfThenElse => srun(body)
		case _: Sum => srun(body)
		case _ =>
			val iter = callsCtx.flatMap(x => condOpt(x) { case SumCtx(_, _, _, resVar) => resVar }).iterator
			val resVar = if (iter.hasNext) iter.next() else raise("there is no next")
			val assignment = dictAssign(body, isTrieBody)
			assignment match {
				case "" => ""
				case _ => s"$resVar$assignment;"
			}
	}

	private def dictAssign(body: Exp, isTrieBody: Boolean)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
		body match {
			case DictNode(ArrayBuffer((FieldNode(Sym(tupleVar), column), valueNode))) =>
				val idx = if (isTrieBody) "i" else s"${tupleVar}_off"
				s"[$tupleVar.$column[$idx]]${dictAssign(valueNode, isTrieBody)}"
			case _: Const => s"+= ${srun(body)}"
			case DictNode(Nil) => ""
			case DictNode(ArrayBuffer((_: Sym, _: Const))) => ".push_back(i)"
			case _ => raise(s"not supported sides: $body")
		}
	}

	private def toCpp(tpe: Type): String = tpe match {
		case IntType => "int"
		case StringType => "std::string"
		case DictType(key, value) =>
			val value_cpp = value match {
				case IntType => toCpp(value)
				case DictType(IntType, valueValue) => s"phmap::flat_hash_map<int, ${toCpp(valueValue)}>"
				case DictType(_: RecordType, IntType) => "std::vector<int>"
			}
			s"phmap::flat_hash_map<${toCpp(key)}, $value_cpp>"
		case RecordType(fs) => fs.map(_.tpe).map(toCpp).mkString("std::tuple<", ", ", ">")
		case _ => raise(s"unsupported type in toCpp: $tpe")
	}

	private def docVar(name: String) = s"${name.toUpperCase}_DOC"

	private def typeVar(name: String) = s"${name.toUpperCase}_TYPE"

	private def randomVar() = s"v_${UUID.randomUUID.toString.replace("-", "_")}"
}