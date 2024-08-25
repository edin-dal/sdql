package sdql.backend

import sdql.analysis.TypeInference

package object codegen {
  type TypesCtx = TypeInference.Ctx
  val resultName = "result"
}
