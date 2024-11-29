package sdql.model

import io.circe.generic.extras.Configuration

object CirceConfig { implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames }

case class OptPlanModel(optPlan: List[IntermModel])

case class IntermModel(node: NodeModel, buildPlan: BuildPlanModel, compiledPlan: CompiledPlanModel)

case class NodeModel(node: String, keys: NodeKeysModel, record: List[Int])

case class BuildPlanModel(buildPlan: List[(String, List[List[String]])])

case class CompiledPlanModel(compiledPlan: List[List[RelAttrModel]])

case class NodeKeysModel(keys: List[(Int, RelAttrModel)])

case class RelAttrModel(relation: String, attribute: String)
