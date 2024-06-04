package sdql
package backend

import munit.Assertions.munitPrint
import sdql.ir._

import java.util.UUID
import scala.PartialFunction.condOpt
import scala.collection.mutable.ArrayBuffer

object Compiler {
  private type TypesCtx = TypeInference.Ctx
  private type CallsCtx = List[CallCtx]
  private sealed trait CallCtx
  private case class SumCtx(k:String, v: String, agg: String) extends CallCtx

  private val reDate =  "(^\\d{4})(\\d{2})(\\d{2})$".r

  private def uuid = UUID.randomUUID.toString.replace("-", "_")

  def apply(e: Exp): String = {
    val (mainBody, Some((Sym(name), tpe))) = run(e)(Map(), List())
    val printBody = tpe match {
      case _: DictType =>
        s"""
           |for (const auto &[key, val] : $name) {
           |std::cout << key << ':' << val << std::endl;
           |}
           |""".stripMargin
      case _ if tpe.isScalar =>
        s"std::cout << $name << std::endl;"
    }
    s"""|#include "parallel_hashmap/phmap.h"
        |#include "rapidcsv.h"
        |#include "tuple_helper.h"
        |#include <filesystem>
        |#include <iostream>
        |#include <regex>
        |
        |const auto NO_HEADERS = rapidcsv::LabelParams(-1, -1);
        |const auto SEPARATOR = rapidcsv::SeparatorParams('|');
        |
        |class Values { public: int operator [](int _) const { return 1; } };
        |
        |int main() {
        |$mainBody
        |
        |$printBody
        |}""".stripMargin
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): (String, Option[(Sym, Type)]) = e match {
    case LetBinding(x @ Sym(name), e1, e2) =>
      val cpp_e1 = e1 match {
        case Load(path, tp) =>
          s"""const rapidcsv::Document ${name.toUpperCase}("$path", NO_HEADERS, SEPARATOR);
             |
             |${load(name, path, tp)}""".stripMargin
        case _ =>
          s"auto $name = ${srun(e1)};"
      }
      val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1))
      val (cpp_e2, info) = run(e2)(typesLocal, callsCtx)
      (cpp_e1 + cpp_e2, info)

    // TODO remove type inference code duplication (type inference needs to pass context back)
    case Sum(k, v, e1, e2) =>
      // from e1 infer types of k, v
      val e1Sym = e1 match {
        case s: Sym => s
        case _ => raise(s"only ${Sym.getClass.getSimpleName.init} expressions supported")
      }
      val (kType, vType) = typesCtx.get(e1Sym) match {
        case Some(DictType(k_type, v_type)) => (k_type, v_type)
        case Some(tpe) => raise(
          s"assignment should be from ${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
        )
        case None => raise(s"unknown symbol: $e1Sym")
      }
      // from types of k, v infer type of e2
      var typesLocal = typesCtx ++ Map(k -> kType, v -> vType)
      val tpe = TypeInference.run(e2)(typesLocal)

      val agg = s"v_$uuid"
      typesLocal ++= Map(Sym(agg) -> tpe)
      val init =  tpe match {
        case RealType | IntType => "0"
        case DictType(_, _) => "{}"
        case _ => raise(s"unhandled type: $tpe")
      }

      val callsLocal = List(SumCtx(k=k.name, v=v.name, agg=agg)) ++ callsCtx
      val loopBody = e2 match {
        case _: LetBinding | _: IfThenElse =>
          srun(e2)(typesLocal, callsLocal)
        case _ =>
          ifElseBody(e2)(typesLocal, callsLocal)
      }

      (
        s"""${toCpp(tpe)} $agg($init);
          |const ${e1Sym.name.capitalize} &${k.name} = ${e1Sym.name};
          |constexpr auto ${v.name} = Values();
          |for (int i = 0; i < ${e1Sym.name.toUpperCase()}.GetRowCount(); i++) {
          |$loopBody
          |}""".stripMargin,
        Some(Sym(agg), tpe),
      )

    case IfThenElse(cond, Const(false), Const(true)) =>
      (s"!(${srun(cond)})", None)
    // base case
    case IfThenElse(cond, e1, Const(false)) =>
      e1 match {
        case _: Const | _: Cmp =>
        case _ => raise(s"unexpected class: ${e1.simpleName}")
      }
      (s"${srun(cond)} && ${srun(e1)}", None)
    // recursive case
    case IfThenElse(cond, e1, e2) =>
      val elseBody = e2 match {
        case DictNode(Nil) => ""
        case _ =>
          s""" else {
            |${ifElseBody(e2)}
            |}""".stripMargin
      }
      (s"""if (${srun(cond)}) {${ifElseBody(e1)}\n}$elseBody""".stripMargin, None)

    case Cmp(e1, e2, cmp) =>
      (s"${srun(e1)} $cmp ${srun(e2)}", None)

    case FieldNode(Sym(name), f) =>
      (s"$name.$f[i]", None)

    case Add(e1, Neg(e2)) =>
      (s"(${srun(e1)} - ${srun(e2)})", None)
    case Add(e1, e2) =>
      (s"(${srun(e1)} + ${srun(e2)})", None)

    case Mult(e1, e2) =>
      (s"(${srun(e1)} * ${srun(e2)})", None)

    case Neg(e) =>
      (s"-${srun(e)}", None)

    case Const(DateValue(v)) =>
      val yyyymmdd = reDate.findAllIn(v.toString).matchData.next()
      (s""""${yyyymmdd.group(1)}-${yyyymmdd.group(2)}-${yyyymmdd.group(3)}"""", None)
    case Const(v: String) =>
      (s""""$v"""", None)
    case Const(v) =>
      (v.toString, None)

    case Sym(name) =>
      val iter = callsCtx.flatMap(x => condOpt(x) { case SumCtx(k, v, _) => (k, v) }).iterator
      if (iter.hasNext) {
        val(k, v) = iter.next()
        if (name == k || name == v)
          return (s"$name[i]", None)
      }
      (name, None)

    case DictNode(Nil) =>
      ("", None)
    case DictNode(ArrayBuffer((_, e2: RecNode))) =>
      (srun(e2), None)

    case RecNode(values) =>
      val tpe = TypeInference.run(e) match {
        case tpe: RecordType => tpe
        case tpe => raise(
          s"expression ${e.simpleName} should be of type " +
            s"${RecordType.getClass.getSimpleName.init} not ${tpe.simpleName}"
        )
      }
      (values.map(e => srun(e._2)).mkString(s"${toCpp(tpe)}(", ", ", ")"), None)

    case External(name, args) =>
      import ExternalFunctions._
      args match {
        case ArrayBuffer(field: FieldNode, elem, from) if name == StrIndexOf.SYMBOL =>
          (s"${srun(field)}.find(${srun(elem)}, ${srun(from)})", None)
        case _ => raise(s"unhandled function name: $name")
      }

    case _ => raise(
      f"""Unhandled ${e.simpleName} in
         |${munitPrint(e)}""".stripMargin
    )
  }

  private def load(varName: String, path: String, tp: Type): String = tp match {
    case DictType(RecordType(fs), IntType) =>
      val struct_def = fs.map(attr => s"std::vector<${toCpp(attr.tpe)}> ${attr.name};")
        .mkString(s"struct ${varName.capitalize} {\n", "\n", "\n};\n")
      val struct_init = fs.zipWithIndex.map(
          {
            case (attr, i) => s"${varName.toUpperCase}.GetColumn<${toCpp(attr.tpe)}>($i),"
          }
        )
        .mkString(s"const ${varName.capitalize} ${varName.toLowerCase} {\n", "\n", "\n};\n")
      s"$struct_def\n$struct_init\n"
    case _ =>
      raise(s"`load[$tp]('$path')` only supports the type `{ < ... > -> int }`")
  }

  private def ifElseBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    val iter = callsCtx.flatMap(x => condOpt(x) { case SumCtx(_, _, agg) => agg }).iterator
    val agg = if (iter.hasNext) iter.next() else raise(
      s"${IfThenElse.getClass.getSimpleName.init}"
        + s" inside ${Sum.getClass.getSimpleName.init}"
        + " needs to know aggregation variable name"
    )
    val lhs = typesCtx.get(Sym(agg)) match {
      case Some(t: DictType) => e match {
        case DictNode(ArrayBuffer((RecNode(keys), _))) =>
          val ks = keys.map(_._2).map(x => {srun(x)(Map(), List())}).mkString(", ")
          s"$agg[${toCpp(t.key)}($ks)]"
        case DictNode(ArrayBuffer((e1: FieldNode, _))) =>
          s"$agg[${srun(e1)}]"
        }
      case Some(t) if t.isScalar =>
        agg
      case None => raise(
        s"${IfThenElse.getClass.getSimpleName.init}"
          + s" inside ${Sum.getClass.getSimpleName.init}"
          + " needs to know aggregation variable type"
      )
    }
    e match {
      case DictNode(Nil) => ""
      case DictNode(ArrayBuffer((_: FieldNode, e2))) => s"$lhs += ${srun(e2)};"
      case _ => s"$lhs += ${srun(e)};"
    }
  }

  def srun(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = run(e)._1

  private def toCpp(tpe: ir.Type): String = tpe match {
    case RealType => "double"
    case IntType => "int"
    case StringType | DateType => "std::string"
    case DictType(key, value) => s"phmap::flat_hash_map<${toCpp(key)}, ${toCpp(value)}>"
    case RecordType(attrs) => attrs.map(_.tpe).map(toCpp).mkString("std::tuple<", ", ", ">")
    case tpe => raise(s"unimplemented type: $tpe")
  }
}
