scalaVersion := "2.13.14"

libraryDependencies ++= Seq(
  "com.lihaoyi"   %% "fastparse"      % "3.1.1",
  "junit"          % "junit-dep"      % "4.11"   % "test",
  "org.scalatest"  % "scalatest_2.13" % "3.2.19" % "test",
  "org.scalameta" %% "munit"          % "1.0.0" // for pretty printing expressions
)

// Note: IntelliJ accepts -P but SBT requires no. of threads
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-P32")

// Comment out these global options to enable running the tests from command line
Test / testOptions ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestTPCH0_01"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestTPCH1"),
  // require converting from Parquet to CSV the JOB datasets of https://github.com/SIGMOD23p561/free-join
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestJOBGJ"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestJOBFJ"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestJOBFJSorting"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestJOBFJHybrid"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestJOBGJHybrid"),
  // require converting from Parquet to CSV the LSQB datasets of https://github.com/remysucre/gj-vs-binary
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBGJ0_1"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBFJ0_1"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBGJ0_3"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBFJ0_3"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBGJ1"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBFJ1"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBGJ3"),
  Tests.Argument(TestFrameworks.ScalaTest, "-l", "TestLSQBFJ3")
)

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
  "-Wvalue-discard"
)
