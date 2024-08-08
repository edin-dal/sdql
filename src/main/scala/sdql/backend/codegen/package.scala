package sdql.backend

import sdql.analysis.TypeInference

import scala.collection.Seq

package object codegen {
  type TypesCtx = TypeInference.Ctx
  type CallsCtx = Seq[CallCtx]

  val resultName = "result"
}
