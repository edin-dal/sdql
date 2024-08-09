package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.raise

import scala.PartialFunction.condOpt

object ReadUtils {
  def cppCsvs(exps: Seq[Exp]): String = {
    val pathNameType = exps
      .flatMap(
        e =>
          iterExps(e)
            .flatMap(
              e =>
                condOpt(e) {
                  case LetBinding(Sym(name), load @ Load(path, tp: RecordType), _) if TypeInference.isColumnStore(tp) =>
                    val recordType = (load: @unchecked) match { case Load(_, recordType: RecordType) => recordType }
                    (path, name, recordType)
              }
          )
      )
      .distinct
      .sortBy(_._2)

    val csvConsts = pathNameType.map({ case (path, name, _) => makeCsvConst(name, path) }).mkString("", "\n", "\n")
    val tuples = pathNameType
      .map({
        case (_, name, recordType) =>
          val init = makeTupleInit(name, recordType)
          s"auto ${name.toLowerCase} = ${cppType(recordType)}($init);\n"
      })
      .mkString("\n")

    Seq(csvConsts, tuples).mkString("\n")
  }

  private def makeCsvConst(name: String, path: String) =
    s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR);"""

  private def makeTupleInit(name: String, recordType: RecordType) = {
    assert(recordType.attrs.last.name == "size")
    val attrs = recordType.attrs
      .dropRight(1)
      .map(attr => (attr.tpe: @unchecked) match { case DictType(IntType, vt, Vec) => Attribute(attr.name, vt) })

    (attrs.zipWithIndex.map({
      case (Attribute(attr_name, tpe), i) =>
        s"/* $attr_name */" ++ (tpe match {
          case DateType =>
            s"dates_to_numerics(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
          case StringType(Some(maxLen)) =>
            s"strings_to_varchars<$maxLen>(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
          case _ =>
            s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i)"
        })
    }) ++ Seq(s"/* size */static_cast<${cppType(IntType)}>(${name.toUpperCase}_CSV.GetRowCount())"))
      .mkString(",\n")
  }

  private def iterExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: RangeNode | _: Load => Iterator()
        // 1-ary
        case Neg(e)          => iterExps(e)
        case FieldNode(e, _) => iterExps(e)
        case Promote(_, e)   => iterExps(e)
        case Unique(e)       => iterExps(e)
        // 2-ary
        case Add(e1, e2)           => iterExps(e1) ++ iterExps(e2)
        case Mult(e1, e2)          => iterExps(e1) ++ iterExps(e2)
        case Cmp(e1, e2, _)        => iterExps(e1) ++ iterExps(e2)
        case Sum(_, _, e1, e2)     => iterExps(e1) ++ iterExps(e2)
        case Get(e1, e2)           => iterExps(e1) ++ iterExps(e2)
        case Concat(e1, e2)        => iterExps(e1) ++ iterExps(e2)
        case LetBinding(_, e1, e2) => iterExps(e1) ++ iterExps(e2)
        // 3-ary
        case IfThenElse(e1, e2, e3) => iterExps(e1) ++ iterExps(e2) ++ iterExps(e3)
        // n-ary
        case RecNode(values)   => values.map(_._2).flatMap(iterExps)
        case DictNode(dict, _) => dict.flatMap(x => iterExps(x._1) ++ iterExps(x._2))
        case External(_, args) => args.flatMap(iterExps)
        // unhandled
        case _ => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
      }
    )
}