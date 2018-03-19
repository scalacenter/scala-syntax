lazy val metaV = "3.2.0"

lazy val paiges = project
  .in(file("paiges/core"))
  .settings(
    moduleName := "paiges-core",
    organization := "org.typelevel",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
    ),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
  )

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
      "com.lihaoyi" %% "pprint" % "0.5.2", // for debugging
      "org.scalameta" %% "scalameta" % metaV,
      "org.scalameta" %% "contrib" % metaV
    )
  )
  .dependsOn(paiges)

val utestSettings = Seq(
  testFrameworks := List(
    new TestFramework("org.scalafmt.tests.CustomFramework")
  )
)

// IntegrationTest configuration is not worth the complexity, reusing code across
// configuration is annoying. Easier to create more projects.
lazy val testsShared = project
  .in(file("tests/shared"))
  .settings(utestSettings)
  .settings(
    libraryDependencies ++= List(
      "ch.epfl.scala" %% "scalafix-diff" % "0.5.1",
      "org.scalameta" %% "testkit" % metaV,
      "com.lihaoyi" %% "utest" % "0.6.3",
      "org.scalatest" %% "scalatest" % "3.0.4",
      "com.lihaoyi" %% "fansi" % "0.2.5" % Test
    )
  )
  .dependsOn(format)

lazy val unit = project
  .in(file("tests/unit"))
  .settings(utestSettings)
  .dependsOn(testsShared)

lazy val slow = project
  .in(file("tests/slow"))
  .settings(utestSettings)
  .settings(
    libraryDependencies += "me.tongfei" % "progressbar" % "0.5.5",
    fork in (Test, test) := true,
    cancelable in Global := true,
    javaOptions in (Test, test) ++= {
      val mem =
        if (sys.env.get("CI").isDefined) 4
        else 20

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
