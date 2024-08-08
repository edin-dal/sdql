val scala_version = "2.12.19"

scalaVersion := scala_version

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scala_version,
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "junit" % "junit-dep" % "4.11" % "test",
  "org.scalatest" % "scalatest_2.12" % "3.2.19" % "test",
  "org.scalameta" %% "munit" % "1.0.0" // for pretty printing expressions
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-P32")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
