package sdql.backend

import sdql.analysis.TypeInference
import sdql.ir.*
import sdql.raise

import scala.collection.Seq

package object codegen {
  type TypesCtx = TypeInference.Ctx
  type CallsCtx = Seq[CallCtx]

  val resultName = "result"

  def cppType(tpe: Type): String = tpe match {
    case BoolType                       => "bool"
    case RealType                       => "double"
    case IntType | DateType             => "long"
    case StringType(None)               => "std::string"
    case StringType(Some(maxLen))       => s"VarChar<$maxLen>"
    case DictType(kt, vt, NoHint)       => s"phmap::flat_hash_map<${cppType(kt)}, ${cppType(vt)}>"
    case DictType(kt, IntType, VecDict) => s"vecdict<${cppType(kt)}>"
    case DictType(IntType, vt, Vec)     => s"std::vector<${cppType(vt)}>"
    case RecordType(attrs)              => attrs.map(_.tpe).map(cppType).mkString("std::tuple<", ", ", ">")
    case tpe                            => raise(s"unimplemented type: $tpe")
  }
}
