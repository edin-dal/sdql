package sdql.backend.codegen

import sdql.analysis.TypeInference

import sdql.ir.*
import sdql.raise

object SumUtils {
  def run(e: Sum)(implicit typesCtx: TypesCtx, isTernary: Boolean): String = e match {
    case Sum(k, v, e1, e2) =>
      val (_, typesLocal) = TypeInference.sumInferTypeAndCtx(k, v, e1, e2)
      val body            = CppCodegen.run(e2)(typesLocal, isTernary)
      val forBody = e1 match {
        case _: RangeNode => s"${cppType(IntType)} ${k.name} = 0; ${k.name} < ${CppCodegen.run(e1)}; ${k.name}++"
        case _ =>
          val iterable = CppCodegen.run(e1)(typesLocal, isTernary)
          val head = TypeInference.run(e1)(typesLocal) match {
            case DictType(_, _, _: PHmap)         => s"&[${k.name}, ${v.name}]"
            case DictType(_, _, _: SmallVecDict)  => s"&${k.name}"
            case DictType(_, _, _: SmallVecDicts) => s"${k.name}"
            case t                                => raise(s"unexpected: ${t.prettyPrint}")
          }
          s"auto $head : $iterable"
      }
      s"for ($forBody) { $body }"
  }

  def getAggregation(tp: Type): Aggregation = tp match {
    case TropicalSemiRingType(false, false, _) => MinAgg
    case TropicalSemiRingType(true, false, _)  => MaxAgg
    case TropicalSemiRingType(_, true, _)      => ProdAgg
    case _                                     => raise(s"unexpected: ${tp.prettyPrint}")
  }
}
