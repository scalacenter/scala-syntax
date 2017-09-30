lazy val format = project.settings(
  moduleName := "scala-format",
  scalaVersion := "2.12.3",
  libraryDependencies ++= List(
    "org.scalameta" %% "scalameta" % "2.0.1",
    "org.typelevel" %% "paiges-core" % "0.1.2"
    )
)
