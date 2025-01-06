package sdql.model

import io.circe.DecodingFailure
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto.*
import io.circe.parser.decode

import java.nio.file.Path

object CirceConfig { implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames }

case class OptPlanModel(optPlan: List[IntermModel])

case class IntermModel(node: NodeModel, buildPlan: BuildPlanModel, compiledPlan: CompiledPlanModel)

case class NodeModel(node: String, keys: NodeKeysModel, record: List[Int])

case class BuildPlanModel(buildPlan: List[(String, List[List[String]])])

case class CompiledPlanModel(compiledPlan: List[List[RelAttrModel]])

case class NodeKeysModel(keys: List[(Int, RelAttrModel)])

case class RelAttrModel(relation: String, attribute: String)

object OptPlanModel {
  import CirceConfig.*

  def fromPath(path: Path): OptPlanModel = {
    val source         = scala.io.Source.fromFile(path.toFile)
    val jsonString     =
      try source.mkString
      finally source.close()
    val decodedOptPlan = decode[OptPlanModel](jsonString)
    decodedOptPlan match {
      case Left(e: DecodingFailure) =>
        throw new Exception(
          s"Failed to parse JSON from $path: ${e.message} at ${e.history.map(_.toString).mkString(" -> ")}"
        )
      case Left(e)                  => throw new Exception(s"Failed to parse JSON from $path: $e")
      case Right(plan)              => plan
    }
  }
}
