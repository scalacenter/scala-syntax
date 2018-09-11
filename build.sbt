lazy val metaV = "4.0.0-M11"

lazy val format = project
  .settings(
    moduleName := "scala-format",
    assemblyJarName in assembly := "scalafmt.jar",
    mainClass.in(assembly) := Some("org.scalafmt.Format"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= List(
      "com.lihaoyi" %% "pprint" % "0.5.2", // for debugging
      "org.scalameta" %% "paiges" % "0.2.2-SNAP1",
      "org.scalameta" %% "scalameta" % metaV,
      "org.scalameta" %% "contrib" % metaV
    )
  )

// IntegrationTest configuration is not worth the complexity, reusing code across
// configuration is annoying. Easier to create more projects.
lazy val testsShared = project
  .in(file("tests/shared"))
  .settings(
    libraryDependencies ++= List(
      "ch.epfl.scala" %% "scalafix-diff" % "0.5.1",
      "org.scalameta" %% "testkit" % metaV,
      "com.lihaoyi" %% "utest" % "0.6.3",
      "org.scalatest" %% "scalatest" % "3.0.4",
      "com.lihaoyi" %% "fansi" % "0.2.5" % Test
      // "com.github.masseguillaume" %% "scalameta-structure" % "0.1.1"
    )
  )
  .dependsOn(format)

lazy val unit = project
  .in(file("tests/unit"))
  .dependsOn(testsShared)

lazy val slow = project
  .in(file("tests/slow"))
  .settings(
    libraryDependencies += "me.tongfei" % "progressbar" % "0.5.5",
    fork in (Test, test) := true,
    fork in (Test, testOnly) := true,
    fork in (Test, testQuick) := true,
    cancelable in Global := true,
    javaOptions in Test ++= {
      val mem =
        if (sys.env.get("CI").isDefined) "4"
        else sys.env.get("SLOWMEM").getOrElse("20")

      Seq(
        "-Xss20m",
        "-Xms4G",
        s"-Xmx${mem}G",
        "-XX:ReservedCodeCacheSize=1024m",
        "-XX:+TieredCompilation",
        "-XX:+CMSClassUnloadingEnabled"
      )
    }
  )
  .dependsOn(testsShared)
