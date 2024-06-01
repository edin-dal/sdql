package sdql
package backend
import munit.Assertions.munitPrint
import sdql.ir._

import java.util.UUID
import scala.collection.mutable.ArrayBuffer

object Compiler {
  type Type = ir.Type
  type Var = Sym
  type Ctx = Map[Sym, Type]

  private val reFilename = "^(.+/)*(.+)\\.(.+)$".r
  private val reDate =  "(^\\d{4})(\\d{2})(\\d{2})$".r

  private def uuid = UUID.randomUUID.toString.replace("-", "_")

  // TODO
  //  for (const auto &[fst, snd] : v_()) {
  //    std::cout << fst << ':' << snd << std::endl;
  //  }
  def apply(e: Exp): String =
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
        |${run(e)(Map())}
        |}""".stripMargin

  def run(e: Exp, maybeName: Option[String] = None)(implicit ctx: Ctx): String = e match {
    case LetBinding(x @ Sym(name), e1, e2) =>
//      println("*" * 80)
//      println(s"${e.getClass} /")
//      println("*" * 40)
//      println(s"x: $x")
//      println("*" * 40)
//      println(munitPrint(e1))
//      println("*" * 40)
//      println(munitPrint(e2))
//      println("*" * 40)
//      println(s"/ ${e.getClass}")
//      println("*" * 80)
      run(e1) + run(e2, Some(name))(ctx ++ Map(x -> TypeInference.run(e1)))

    case Sum(k, v, e1, e2) =>
      // infer types of k, v from e1
      val e1_sym = e1 match {
        case s: Sym => s
        case _ => raise(s"only ${Sym.getClass.getSimpleName.init} expressions supported")
      }
      val (kType, vType) = ctx.get(e1_sym) match {
        case _ @ Some(DictType(k_type, v_type)) => (k_type, v_type)
        case Some(tpe) => raise(
          s"assignment should be from ${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
        )
        case None => raise(s"unknown symbol: $e1_sym")
      }

      // Note: k and v are bound to local scope, not global
      var localCtx = ctx ++ Map(k -> kType, v -> vType)

      val tpe = TypeInference.run(e2)(localCtx)
      val agg = s"v_$uuid"
      localCtx ++= Map(Sym(agg) -> tpe)
      val init =  tpe match {
        case RealType | IntType => "0"
        case DictType(_, _) => "{}"
        case _ => raise(s"unhandled type: $tpe")
      }
      val loopBody = e2 match {
        case _: IfThenElse => run(e2, Some(agg))(localCtx)
        case _ => run(e2)(localCtx)
      }
      val name = maybeName match {
        case Some(name) => name
        case None => raise(
          s"${Sum.getClass.getSimpleName.init}"
            + s"inside ${LetBinding.getClass.getSimpleName.init}"
            + " needs to know binding variable name"
        )
      }
      s"""${toCpp(tpe)} $agg($init);
          |const ${name.capitalize} &li = $name;
          |for (int i = 0; i < ${e1_sym.name.toUpperCase()}.GetRowCount(); i++) {
          |$loopBody
          |}""".stripMargin

    // base case
    case IfThenElse(cond, e1, Const(false)) =>
      e1 match {
        case _: Const | _: Cmp =>
        case _ => raise(s"unexpected class: ${e1.simpleName}")
      }
      s"${run(cond)} && ${run(e1)}"
    // recursive case
    case IfThenElse(cond, e1, e2) =>
      val name = maybeName match {
        case Some(name) => name
        case None => raise(
          s"${IfThenElse.getClass.getSimpleName.init}"
            + s"inside ${Sum.getClass.getSimpleName.init}"
            + " needs to know aggregation variable name"
        )
      }
      s"""if (${run(cond)}) {${ifElseBody(e1, name)}
         |} else {${ifElseBody(e2, name)}
         |}""".stripMargin

    case Cmp(e1, e2, cmp) =>
      s"${run(e1)} $cmp ${run(e2)}"

    // TODO hardcoded [i]
    case FieldNode(Sym(name), f) =>
      s"$name.$f[i]"

    case Add(e1, Neg(e2)) =>
      s"(${run(e1)} - ${run(e2)})"
    case Add(e1, e2) =>
      s"(${run(e1)} + ${run(e2)})"

    case Mult(e1, e2) =>
      s"(${run(e1)} * ${run(e2)})"

    case Neg(e) =>
      s"-${run(e)}"

    case Const(DateValue(v)) =>
      val yyyymmdd = reDate.findAllIn(v.toString).matchData.next()
      s""""${yyyymmdd.group(1)}-${yyyymmdd.group(2)}-${yyyymmdd.group(3)}""""
    case Const(v) =>
      v.toString

    case DictNode(Nil) =>
      ""
    case DictNode(ArrayBuffer((_, RecNode(values)))) =>
      assert(values.isInstanceOf[ArrayBuffer[(Field, Exp)]])
      val tpe = TypeInference.run(e) match {
        case dictTpe: DictType => dictTpe.value
        case tpe => raise(
          s"expression ${e.simpleName} should be of type " +
            s"${DictType.getClass.getSimpleName.init} not ${tpe.simpleName}"
        )
      }
      values.map(e => run(e._2)).mkString(s"${toCpp(tpe)}(", ", ", ")")

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
          s"$csv\n$struct_def\n$struct_init\n"
        case _ =>
          raise(s"`load[$tp]('$path')` only supports the type `{ < ... > -> int }`")
      }

    case _ => raise(
      f"""Unhandled ${e.simpleName} in
         |${munitPrint(e)}""".stripMargin
    )
  }

  private def ifElseBody(e: Exp, name: String)(implicit ctx: Ctx): String = {
    val lhs = ctx.get(Sym(name)) match {
      // TODO hardcoded li.l_returnflag, li.l_linestatus and [i]
      case Some(t: DictType) =>
        s"$name[${toCpp(t.key)}(li.l_returnflag[i], li.l_linestatus[i])]"
      case Some(t) if t.isScalar =>
        name
      case None => raise(
        s"${IfThenElse.getClass.getSimpleName.init}"
          + s"inside ${Sum.getClass.getSimpleName.init}"
          + " needs to know aggregation variable type"
      )
    }
    e match {
      case DictNode(Nil) => ""
      case _ => s"\n$lhs += ${run(e, Some(name))};"
    }
  }

  private def toCpp(tpe: ir.Type): String = tpe match {
    case RealType => "double"
    case IntType => "int"
    case StringType | DateType => "std::string"
    case DictType(key, value) => s"phmap::flat_hash_map<${toCpp(key)}, ${toCpp(value)}>"
    case RecordType(attrs) => attrs.map(_.tpe).map(toCpp).mkString("std::tuple<", ", ", ">")
    case tpe => raise(s"unimplemented type: $tpe")
  }
}
