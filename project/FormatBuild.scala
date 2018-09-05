import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object FormatBuild extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins = JvmPlugin

  override def globalSettings: Seq[Def.Setting[_]] = List(
    scalaVersion := "2.12.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked"
    ),
    testFrameworks := List(
      new TestFramework("scala.meta.internal.prettyprinters.CustomFramework")
    )
  )

}
