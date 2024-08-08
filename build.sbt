val scalaVersion_ = "2.13.14"

scalaVersion := scalaVersion_

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion_,
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "junit" % "junit-dep" % "4.11" % "test",
  "org.scalatest" % "scalatest_2.13" % "3.2.19" % "test",
  "org.scalameta" %% "munit" % "1.0.0" // for pretty printing expressions
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-P32")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xsource:3", "-Xfatal-warnings")
