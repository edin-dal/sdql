val scala_version = "2.12.19"

scalaVersion := scala_version

libraryDependencies ++= Seq(
  "org.scala-lang"    %  "scala-reflect" % scala_version,
  "com.lihaoyi"      %% "fastparse"     % "3.1.0",
  "junit"             % "junit-dep"      % "4.11"  % "test",
  "org.scalatest"     % "scalatest_2.12" % "3.1.4" % "test"
)