package org.scalafmt

import org.scalafmt.internal.Printer

trait Options {
  def maxColumn: Int
}
case class InternalOptions(
    maxColumn: Int = 80
) extends Options

object Options {
  val default: Options = InternalOptions()
}
object Format {
  def format(code: String): String = {
    format(code, Options.default)
  }
  def format(code: String, options: Options): String = {
    val printer = new Printer(code, options)
    printer.print(printer.parsed).render(options.maxColumn)
  }
}
