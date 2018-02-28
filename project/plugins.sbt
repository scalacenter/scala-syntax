addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "1.2.0")
addSbtPlugin(
  "io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version
)
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.5.10")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")