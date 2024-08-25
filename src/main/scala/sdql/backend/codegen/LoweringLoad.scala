package sdql.backend.codegen

import sdql.analysis.TypeInference
import sdql.backend.codegen.LoweringType.cppType
import sdql.ir.*
import sdql.raise

import scala.PartialFunction.condOpt

object LoweringLoad {
  def cppCsvs(e: Exp): String = {
    val pathNameTypeSkip = iterExps(e).flatMap(extract).toSeq.distinct.sortBy(_._2)

    val csvConsts =
      pathNameTypeSkip.map({ case (path, name, _, _) => makeCsvConst(name, path) }).mkString("\n", "\n", "\n")
    val tuples = pathNameTypeSkip
      .map({
        case (_, name, recordType, skipCols) =>
          val init = makeTupleInit(name, recordType, skipCols)
          s"auto ${name.toLowerCase} = ${cppType(recordType, noTemplate = true)}($init);\n"
      })
      .mkString("\n")

    Seq(csvConsts, tuples).mkString("\n")
  }

  private def extract(e: Exp) = condOpt(e) {
    case LetBinding(Sym(name), load @ Load(path, tp: RecordType, _), _) if TypeInference.isColumnStore(tp) =>
      val recordType = (load: @unchecked) match { case Load(_, recordType: RecordType, _) => recordType }
      val skipCols: Set[String] =
        (load: @unchecked) match { case Load(_, _, skipCols) => skipCols.toSkipColsSet }
      (path, name, recordType, skipCols)
  }

  private def makeCsvConst(name: String, path: String) =
    s"""const rapidcsv::Document ${name.toUpperCase}_CSV("../$path", NO_HEADERS, SEPARATOR);"""

  private def makeTupleInit(name: String, recordType: RecordType, skipCols: Set[String]) = {
    assert(recordType.attrs.last.name == "size")
    val attrs = recordType.attrs
      .dropRight(1)
      .map(attr => (attr.tpe: @unchecked) match { case DictType(IntType, vt, Vec(None)) => Attribute(attr.name, vt) })

    val readCols = attrs.zipWithIndex.filter { case (attr, _) => !skipCols.contains(attr.name) }
      .map({
        case (Attribute(attr_name, tpe), i) =>
          s"/* $attr_name */" ++ (tpe match {
            case DateType =>
              s"dates_to_numerics(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
            case StringType(Some(maxLen)) =>
              s"strings_to_varchars<$maxLen>(" + s"${name.toUpperCase}_CSV.GetColumn<${cppType(StringType())}>($i)" + ")"
            case _ =>
              s"${name.toUpperCase}_CSV.GetColumn<${cppType(tpe)}>($i)"
          })
      })
    val readSize =
      if (skipCols.contains("size")) Seq()
      else Seq(s"/* size */static_cast<${cppType(IntType)}>(${name.toUpperCase}_CSV.GetRowCount())")
    (readCols ++ readSize).mkString(",\n")
  }

  private def iterExps(e: Exp): Iterator[Exp] =
    Iterator(e) ++ (
      e match {
        // 0-ary
        case _: Sym | _: Const | _: Load => Iterator()
        // 1-ary
        case Neg(e)          => iterExps(e)
        case FieldNode(e, _) => iterExps(e)
        case Promote(_, e)   => iterExps(e)
        case RangeNode(e)    => iterExps(e)
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
        case RecNode(values)               => values.map(_._2).flatMap(iterExps)
        case DictNode(map, PHmap(Some(e))) => map.flatMap(x => iterExps(x._1) ++ iterExps(x._2)) ++ iterExps(e)
        case DictNode(map, _)              => map.flatMap(x => iterExps(x._1) ++ iterExps(x._2))
        case External(_, args)             => args.flatMap(iterExps)
        // LLQL
        case Initialise(_, _, e) => iterExps(e)
        case Update(e, _, _)     => iterExps(e)
        case Modify(e, _)        => iterExps(e)
        case _                   => raise(f"unhandled ${e.simpleName} in\n${e.prettyPrint}")
      }
    )
}
