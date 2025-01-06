package sdql.model

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.{ Files, Paths }
import scala.jdk.CollectionConverters.*

class OptPlanModelTest extends AnyFlatSpec with ParallelTestExecution {

  it should "decode JOB FJ json plans" in decodeJobJsonPlans("plans/job/fj")

  it should "decode JOB GJ json plans" in decodeJobJsonPlans("plans/job/gj")

  private def decodeJobJsonPlans(path: String): Unit = {
    val paths     = Paths.get(System.getProperty("user.dir")).resolve(path)
    val jsonFiles = Files.list(paths).iterator().asScala.filter(_.toString.endsWith(".json"))
    jsonFiles.foreach(path => OptPlanModel.fromPath(path))
  }
}
