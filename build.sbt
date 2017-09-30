lazy val format = project.settings(
  moduleName := "scala-format",
  scalaVersion := "2.12.3",
  libraryDependencies ++= List(
    "org.scalameta" %% "scalameta" % "2.0.1",
    "org.typelevel" %% "paiges-core" % "0.2.0",
    "org.scalameta" %% "testkit" % "2.0.1" % Test,
    "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    "com.lihaoyi" %% "utest" % "0.5.4" % Test
  ),
  testFrameworks := List(
    new TestFramework("utest.runner.Framework")
  )
)
