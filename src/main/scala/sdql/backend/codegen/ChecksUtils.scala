package sdql.backend.codegen

import sdql.ir.{ Exp, IfThenElse, LetBinding, Sum }

import scala.PartialFunction.{ cond, condOpt }

sealed trait CallCtx
case class LetCtx(name: String) extends CallCtx
case object SumStart            extends CallCtx
case object SumEnd              extends CallCtx

object ChecksUtils {
  def checkIsSumBody(e: Exp)(implicit callsCtx: CallsCtx): Boolean =
    !cond(e) { case _: LetBinding | _: IfThenElse | _: Sum => true } && checkActiveSumCtx

  private def checkActiveSumCtx(implicit callsCtx: CallsCtx) =
    callsCtx.indexWhere(x => cond(x) { case SumStart => true }) match {
      case -1 => false
      case start =>
        callsCtx.indexWhere(x => cond(x) { case _: LetCtx | SumEnd => true }) match {
          case -1  => true
          case end => start < end
        }
    }

  def aggregationName(implicit callsCtx: CallsCtx): String =
    callsCtx.flatMap(x => condOpt(x) { case LetCtx(name) => name }).head
}
