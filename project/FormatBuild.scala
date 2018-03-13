import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object FormatBuild extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins = JvmPlugin

  override def globalSettings: Seq[Def.Setting[_]] = List(
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq(
      "-Xlint", "-deprecation", "-encoding", "UTF-8", "-feature", "-unchecked",
      "-Ywarn-unused-import", "-Ywarn-unused:locals", "-Ywarn-unused:privates"
    ),
    testFrameworks := List(
      new TestFramework("org.scalafmt.tests.CustomFramework")
    )
  )

}
