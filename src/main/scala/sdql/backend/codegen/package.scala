package sdql.backend

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.raise

import scala.collection.Seq

package object codegen {
  type TypesCtx = TypeInference.Ctx
  type CallsCtx = Seq[CallCtx]

  val resultName = "result"

  def cppType(tpe: Type, noTemplate: Boolean = false): String = tpe match {
    case DictType(kt, vt, NoHint) =>
      val template = if (noTemplate) "" else s"<${cppType(kt)}, ${cppType(vt)}>"
      s"phmap::flat_hash_map$template"
    case DictType(kt, IntType, VecDict) =>
      val template = if (noTemplate) "" else s"<${cppType(kt)}>"
      s"vecdict$template"
    case DictType(IntType, vt, Vec) =>
      val template = if (noTemplate) "" else s"<${cppType(vt)}>"
      s"std::vector$template"
    case RecordType(attrs) =>
      val template = if (noTemplate) "" else attrs.map(_.tpe).map(cppType(_)).mkString("<", ", ", ">")
      s"std::tuple$template"
    case BoolType                 => "bool"
    case RealType                 => "double"
    case IntType | DateType       => "long"
    case StringType(None)         => "std::string"
    case StringType(Some(maxLen)) => s"VarChar<$maxLen>"
    case tpe                      => raise(s"unimplemented type: $tpe")
  }
}
