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
    case DictType(kt, IntType, SmallVecDict(size)) =>
      val template = if (noTemplate) "" else s"<${cppType(kt)}, $size>"
      s"smallvecdict$template"
    case DictType(rt: RecordType, IntType, SmallVecDicts(size)) =>
      val template = if (noTemplate) "" else s"<$size, ${recordParams(rt)}>"
      s"smallvecdicts$template"
    case DictType(IntType, vt, _: Vec) =>
      val template = if (noTemplate) "" else s"<${cppType(vt)}>"
      s"std::vector$template"
    case rt: RecordType =>
      val template = if (noTemplate) "" else s"<${recordParams(rt)}>"
      s"std::tuple$template"
    case BoolType                 => "bool"
    case RealType                 => "double"
    case IntType | DateType       => "long"
    case StringType(None)         => "std::string"
    case StringType(Some(maxLen)) => s"VarChar<$maxLen>"
    case tpe                      => raise(s"unimplemented type: $tpe")
  }
  private def recordParams(rt: RecordType) = rt match {
    case RecordType(attrs) => attrs.map(_.tpe).map(cppType(_)).mkString(", ")
  }
}
