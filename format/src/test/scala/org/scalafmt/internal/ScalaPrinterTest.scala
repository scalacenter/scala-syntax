package org.scalafmt.tests

import scala.meta.dialects
import scala.meta.internal.ScalametaInternal
import scala.meta.parsers.Parse
import org.scalafmt.Format
import org.scalafmt.InternalOptions
import org.scalafmt.Options
import org.scalameta.logger
import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException

trait DiffSuite extends FunSuiteLike {
  def assertNoDiff(
      obtained: String,
      expected: String,
      title: String = ""
  ): Boolean = {
    val result = compareContents(obtained, expected)
    if (result.isEmpty) true
    else {
      throw DiffFailure(title, expected, obtained, result)
    }
  }

  private def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  private case class DiffFailure(
      title: String,
      expected: String,
      obtained: String,
      diff: String
  ) extends TestFailedException(
        title + "\n" + error2message(obtained, expected),
        3
      )

  private def error2message(obtained: String, expected: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) {
      sb.append(
        s"""#${header("Obtained")}
           #${stripTrailingWhitespace(obtained)}
           #
            #""".stripMargin('#')
      )
    }
    sb.append(
      s"""#${header("Diff")}
         #${stripTrailingWhitespace(compareContents(obtained, expected))}"""
        .stripMargin('#')
    )
    sb.toString()
  }

  private def stripTrailingWhitespace(str: String): String =
    str.replaceAll(" \n", "∙\n")

  private def compareContents(original: String, revised: String): String =
    compareContents(original.trim.split("\n"), revised.trim.split("\n"))

  private def compareContents(
      original: Seq[String],
      revised: Seq[String]
  ): String = {
    import collection.JavaConverters._
    def trim(lines: Seq[String]) = lines.asJava
    val diff = difflib.DiffUtils.diff(trim(original), trim(revised))
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "original",
          "revised",
          original.asJava,
          diff,
          1
        )
        .asScala
        .drop(3)
        .mkString("\n")
  }
}

class ScalaPrinterTest extends DiffSuite {

  val defaultOptions =
    InternalOptions(100).copy(dialect = dialects.Sbt1, parser = Parse.parseStat)
  def check(original: String, options: Options = defaultOptions): Unit = {
    test(logger.revealWhitespace(original)) {
      val obtained = Format.format(original, options)
      import scala.meta._
      val expected =
        ScalametaInternal
          .resetOrigin(
            options.parser(Input.String(original), options.dialect).get
          )
          .syntax
      assertNoDiff(obtained, expected)
    }
  }

  check("package a.b", Options.default)
  check("import a.b, c._, d.{e => f, g => _}")
  check("trait a[A >: B <: C] { self: D =>}")
  check("def a")
  check("var a, b: Int")
  check("val a, b: Int")
  check("val a_ : Int")
  check("def a[A:B]")
  check("def a[A<%B]")
  check("def a = a[T]")
  check("val a = return {a;b}")
  check("var a = a.b(c:_*) + d")
  check("""if (true) 1 else 2""")
  check("""for (a <- b) a""")
  check("for (a <- b; _ = a; if c) yield a")
  check("try a catch b")
  check("try a catch { case _: A => } finally c")
  check("a match { case A(b @ B()) if b => }")
  check("class A extends { var x = 2 } with B")
  check(
    "private[a] final case class A[A]private(a:A)(b: B) extends A(1) with B[C]"
  )
  check("def this(a: A) = this(a)")
}
