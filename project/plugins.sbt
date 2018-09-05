addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.7")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "3.0.0")
addSbtPlugin(
  "io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version
)
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
