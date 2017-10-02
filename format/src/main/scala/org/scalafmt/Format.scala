package org.scalafmt

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.dialects
import scala.meta.parsers.Parse
import scala.util.control.NonFatal
import org.langmeta.inputs.Input
import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath
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
  def main(args: Array[String]): Unit = {
    val n = new AtomicInteger()
    val paths: Seq[AbsolutePath] =
      if (args.isEmpty)
        FileIO
          .listAllFilesRecursively(PathIO.workingDirectory)
          .filter(f => PathIO.extension(f.toNIO) == "scala")
      else args.map(AbsolutePath(_))
    paths.par.foreach { path =>
      try {
        val in = FileIO.slurp(path, StandardCharsets.UTF_8)
        val formatted = format(in)
        Files.write(path.toNIO, formatted.getBytes)
//        print("+")
      } catch {
        case e: MatchError =>
          println(e.getMessage().replaceFirst(".* of class", ""))
        case NonFatal(e) =>
//          print("-")
          n.incrementAndGet()
//          e.setStackTrace(e.getStackTrace.take(10))
//          e.printStackTrace()
      }
    }
    println(s"ERROR: ${n.get()}")
  }
  def format(code: String): String = {
    format(code, Options.default)
  }
  def format(code: String, options: Options): String = {
    val printer = new ScalaPrinter(Input.String(code), options)
    printer.print(printer.root).render(options.maxColumn)
  }
}
