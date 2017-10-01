package org.scalafmt

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.dialects
import scala.meta.parsers.Parse
import org.langmeta.inputs.Input
import org.scalafmt.internal.ScalaPrinter

trait Options {
  def maxColumn: Int
  def parser: Parse[_ <: Tree]
  def dialect: Dialect
}

case class InternalOptions(
    maxColumn: Int = 80,
    parser: Parse[_ <: Tree] = Parse.parseSource,
    dialect: Dialect = dialects.Scala212
) extends Options

object Options {
  val default: Options = InternalOptions()
}
object Format {
  def format(code: String): String = {
    format(code, Options.default)
  }
  def format(code: String, options: Options): String = {
    val printer = new ScalaPrinter(Input.String(code), options)
    printer.print(printer.parsed).render(options.maxColumn)
  }
}
