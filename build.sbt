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
    assemblyMergeStrategy in assembly := {
      case PathList("org", "typelevel", "paiges", xs @ _*) =>
        MergeStrategy.first
      case x =>
        assemblyMergeStrategy.in(assembly).value(x)
    },
    mainClass.in(assembly) := Some("org.scalafmt.Format"),
    libraryDependencies ++= List(
      "com.lihaoyi" %% "pprint" % "0.5.2",
      "org.scalameta" %% "scalameta" % "2.0.1",
      "org.scalameta" %% "contrib" % "2.0.1"
    )
  )
  .dependsOn(paiges)

// IntegrationTest configuration is not worth the complexity, reusing code across
// configuration is annoying. Easier to create more projects.
lazy val testsShared = project
  .in(file("tests/shared"))
  .settings(
    libraryDependencies ++= List(
      "ch.epfl.scala" %% "scalafix-diff" % "0.5.1",
      "org.scalameta" %% "testkit" % "2.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1"
    )
  )
  .dependsOn(format)

lazy val unit = project
  .in(file("tests/unit"))
  .dependsOn(testsShared)

lazy val slow = project
  .in(file("tests/slow"))
  .dependsOn(testsShared)
