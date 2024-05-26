package sdql
package backend

import ir._

object Compiler {
  type Value = Any
  type Var = Sym
  type Ctx = Map[Var, Value]

  def apply(e: Exp): Value = run(e)(Map())

  def run(e: Exp)(implicit ctx: Ctx): Nothing = e match {
    case _ => raise(
      f"""Unhandled ${e.getClass.toString} in
      ${munit.Assertions.munitPrint(e)}"""
    )
  }
}
