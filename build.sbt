// use fork of paiges with custom line combinators.
// If these custom line combinators turn out to be useful, then we can try
// to merge them upstream.
lazy val Paiges = RootProject(
  uri(
    "git://github.com/olafurpg/paiges.git#114ec05b4a3099906c9159ccd1357f3b772b4f1d"
  )
)
lazy val paiges = ProjectRef(Paiges.build, "coreJVM")

lazy val format = project
  .settings(
    moduleName := "scala-format",
    assemblyJarName in assembly := "scalafmt.jar",
    scalaVersion := "2.12.3",
    libraryDependencies ++= List(
      "com.lihaoyi" %% "pprint" % "0.5.2",
      "org.scalameta" %% "scalameta" % "2.0.1",
      "org.scalameta" %% "contrib" % "2.0.1",
      "ch.epfl.scala" %% "scalafix-diff" % "0.5.1" % Test,
      "org.scalameta" %% "testkit" % "2.0.1" % Test,
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )
  .dependsOn(paiges)
