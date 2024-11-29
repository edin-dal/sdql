package sdql.model

import io.circe.DecodingFailure
import io.circe.generic.extras.auto.*
import io.circe.parser.*
import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import sdql.model.CirceConfig.*

import java.nio.file.{ Files, Paths }
import scala.jdk.CollectionConverters.*

class OptPlanModelTest extends AnyFlatSpec with ParallelTestExecution {

  it should "decode JOB FJ json plans" in decodeJobJsonPlans("plans/job/fj")

  it should "decode JOB GJ json plans" in decodeJobJsonPlans("plans/job/gj")

  def decodeJobJsonPlans(other: String): Unit = {
    System.getProperty("user.dir")
    val paths     = Paths.get(System.getProperty("user.dir")).resolve(other)
    val jsonFiles = Files.list(paths).iterator().asScala.filter(_.toString.endsWith(".json"))

    jsonFiles.foreach { path =>
      val source     = scala.io.Source.fromFile(path.toFile)
      val jsonString =
        try source.mkString
        finally source.close()

      val decodedOptPlan = decode[OptPlanModel](jsonString)

      decodedOptPlan match {
        case Left(e: DecodingFailure) =>
          fail(s"Failed to parse JSON from $path: ${e.message} at ${e.history.map(_.toString).mkString(" -> ")}")
        case Left(e)                  => fail(s"Failed to parse JSON from $path: $e")
        case Right(_)                 =>
      }
    }
  }
}
