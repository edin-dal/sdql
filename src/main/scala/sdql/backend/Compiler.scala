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
  private case class LetBindingCtx(lhs: String) extends CallCtx
  private case class SumCtx(agg: String) extends CallCtx

  private val reFilename = "^(.+/)*(.+)\\.(.+)$".r
  private val reDate =  "(^\\d{4})(\\d{2})(\\d{2})$".r

  private def uuid = UUID.randomUUID.toString.replace("-", "_")

  def apply(e: Exp): String = {
    val (mainBody, Some((Sym(name), tpe))) = run(e)(Map(), List())
    val printBody = tpe match {
      case _: DictType =>
        s"""
           |for (const auto &[fst, snd] : $name) {
           |std::cout << fst << ':' << snd << std::endl;
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
        |int main() {
        |$mainBody
        |
        |$printBody
        |}""".stripMargin
  }

  def run(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): (String, Option[(Sym, Type)]) = e match {
    case LetBinding(x @ Sym(name), e1, e2) =>
//      println("*" * 80)
//      println(s"${e.getClass} /")
//      println("*" * 40)
//      println(s"x: $x")
//      println("*" * 40)
//      println(s"e1: ${munitPrint(e1)}")
//      println("*" * 40)
//      println(s"e2: ${munitPrint(e2)}")
//      println("*" * 40)
//      println(s"/ ${e.getClass}")
//      println("*" * 80)
      val (cpp_e1, _) = run(e1)
      val typesLocal = typesCtx ++ Map(x -> TypeInference.run(e1)(typesCtx))
      val callsLocal = List(LetBindingCtx(lhs = name)) ++ callsCtx
      val (cpp_e2, info @ Some(_)) = run(e2)(typesLocal, callsLocal)
      (cpp_e1 + cpp_e2, info)

    case Sum(k, v, e1, e2) =>
      // infer types of k, v from e1
      val e1_sym = e1 match {
        case s: Sym => s
        case _ => raise(s"only ${Sym.getClass.getSimpleName.init} expressions supported")
      }
      val (kType, vType) = typesCtx.get(e1_sym) match {
        case _ @ Some(DictType(k_type, v_type)) => (k_type, v_type)
        case Some(tpe) => raise(
          s"assignment should be from ${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
        )
        case None => raise(s"unknown symbol: $e1_sym")
      }
      // Note: k and v are bound to local scope, not global
      var typesLocal = typesCtx ++ Map(k -> kType, v -> vType)

      val tpe = TypeInference.run(e2)(typesLocal)
      val agg = s"v_$uuid"
      typesLocal ++= Map(Sym(agg) -> tpe)
      val init =  tpe match {
        case RealType | IntType => "0"
        case DictType(_, _) => "{}"
        case _ => raise(s"unhandled type: $tpe")
      }
      val loopBody = e2 match {
        case _: IfThenElse => srun(e2)(typesLocal, List(SumCtx(agg = agg)) ++ callsCtx)
        case _ => srun(e2)(typesCtx ++ typesLocal, callsCtx)
      }

      val iter = callsCtx.flatMap(x => condOpt(x) { case LetBindingCtx(lhs) => lhs }).iterator
      val name = if (iter.hasNext) iter.next() else raise(
        s"${Sum.getClass.getSimpleName.init}"
          + s"inside ${LetBinding.getClass.getSimpleName.init}"
          + " needs to know binding variable name"
      )
      (
        s"""${toCpp(tpe)} $agg($init);
          |const ${name.capitalize} &li = $name;
          |for (int i = 0; i < ${e1_sym.name.toUpperCase()}.GetRowCount(); i++) {
          |$loopBody
          |}""".stripMargin,
        Some(Sym(agg), tpe),
      )

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

    // TODO hardcoded [i]
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
    case Const(v) =>
      (v.toString, None)

    case DictNode(Nil) =>
      ("", None)
    case DictNode(ArrayBuffer((_, RecNode(values)))) =>
      assert(values.isInstanceOf[ArrayBuffer[(Field, Exp)]])
      val tpe = TypeInference.run(e) match {
        case dictTpe: DictType => dictTpe.value
        case tpe => raise(
          s"expression ${e.simpleName} should be of type " +
            s"${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
        )
      }
      (values.map(e => srun(e._2)).mkString(s"${toCpp(tpe)}(", ", ", ")"), None)

    case Load(path, tp) =>
      tp match {
        case DictType(RecordType(fs), IntType) =>
          val varName = reFilename.findAllIn(path).matchData.next().group(2)
          val csv = s"""const rapidcsv::Document ${varName.toUpperCase}("$path", NO_HEADERS, SEPARATOR);\n"""
          val struct_def = fs.map(attr => s"std::vector<${toCpp(attr.tpe)}> ${attr.name};")
            .mkString(s"struct ${varName.capitalize} {\n", "\n", "\n};\n")
          val struct_init = fs.zipWithIndex.map(
              {
                case (attr, i) => s"${varName.toUpperCase}.GetColumn<${toCpp(attr.tpe)}>($i),"
              }
            )
            .mkString(s"const ${varName.capitalize} ${varName.toLowerCase} {\n", "\n", "\n};\n")
          (s"$csv\n$struct_def\n$struct_init\n", None)
        case _ =>
          raise(s"`load[$tp]('$path')` only supports the type `{ < ... > -> int }`")
      }

    case _ => raise(
      f"""Unhandled ${e.simpleName} in
         |${munitPrint(e)}""".stripMargin
    )
  }

  private def ifElseBody(e: Exp)(implicit typesCtx: TypesCtx, callsCtx: CallsCtx): String = {
    val iter = callsCtx.flatMap(x => condOpt(x) { case SumCtx(agg) => agg }).iterator
    val agg = if (iter.hasNext) iter.next() else raise(
      s"${IfThenElse.getClass.getSimpleName.init}"
        + s"inside ${Sum.getClass.getSimpleName.init}"
        + " needs to know aggregation variable name"
    )
    val lhs = typesCtx.get(Sym(agg)) match {
      case Some(t: DictType) =>
        val keys = e match {
          case DictNode(ArrayBuffer(Tuple2(RecNode(keys), _))) =>
            keys.map(_._2).map(x => {srun(x)(Map(), List())}).mkString(", ")
        }
        s"$agg[${toCpp(t.key)}($keys)]"
      case Some(t) if t.isScalar =>
        agg
      case None => raise(
        s"${IfThenElse.getClass.getSimpleName.init}"
          + s"inside ${Sum.getClass.getSimpleName.init}"
          + " needs to know aggregation variable type"
      )
    }
    e match {
      case DictNode(Nil) => ""
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
