scalaVersion := "2.13.14"

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "fastparse"     % "3.1.1",
  "junit"         % "junit-dep"      % "4.11" % "test",
  "org.scalatest" % "scalatest_2.13" % "3.2.19" % "test",
  "org.scalameta" %% "munit"         % "1.0.0" // for pretty printing expressions
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-P32")

// Mostly from https://alexn.org/blog/2020/05/26/scala-fatal-warnings/#2-activate-all-linting-options
scalacOptions := Seq(
  // Warnings as errors!
  "-Xfatal-warnings",
  // Deprecation options
  "-deprecation",
  "-Xsource:3",
  // Feature options
  "-encoding",
  "utf-8",
  "-explaintypes",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ymacro-annotations",
  // Linting options
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:implicits",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:params",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wvalue-discard",
)
