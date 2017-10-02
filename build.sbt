lazy val format = project.settings(
  moduleName := "scala-format",
  assemblyJarName in assembly := "scalafmt.jar",
  scalaVersion := "2.12.3",
  libraryDependencies ++= List(
    "com.lihaoyi" %% "pprint" % "0.5.2",
    "org.scalameta" %% "scalameta" % "2.0.1",
    "org.scalameta" %% "contrib" % "2.0.1",
    "org.typelevel" %% "paiges-core" % "0.2.0",
    "ch.epfl.scala" %% "scalafix-diff" % "0.5.1" % Test,
    "org.scalameta" %% "testkit" % "2.0.1" % Test,
    "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
)
