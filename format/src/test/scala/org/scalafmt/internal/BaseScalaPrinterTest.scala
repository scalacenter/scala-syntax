package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.dialects
import scala.meta.internal.ScalametaInternal
import scala.meta.parsers.Parse
import scala.meta.testkit.StructurallyEqual
import scalafix.diff.DiffUtils
import org.langmeta.inputs.Input
import org.scalafmt.Format
import org.scalafmt.InternalOptions
import org.scalafmt.Options
import org.scalameta.logger

abstract class BaseScalaPrinterTest extends DiffSuite {

  val defaultOptions = InternalOptions(100).copy(
    dialect = dialects.Sbt1.copy(
      allowTypeLambdas = true,
      allowAndTypes = true,
      allowImplicitFunctionTypes = true
    ),
    parser = Parse.parseStat
  )
  def checkType(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseType))
  }
  def checkPat(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parsePat))
  }

  def checkCase(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseCase))
  }

  def checkSource(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(
      original,
      options.copy(parser = Parse.parseSource, dialect = dialects.Scala212)
    )
  }

  def checkEnumerator(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseEnumerator))
  }

  def check(original: String, options: Options = defaultOptions): Unit = {
    check(original, original, options)
  }

  def check(original: String, expected: String): Unit = {
    check(original, expected, defaultOptions)
  }

  def isSameTree(filename: String, a: Tree, b: Tree): Either[String, Unit] =
    StructurallyEqual(a, b).left.map(_ => getDiff(filename, a, b))

  def check(
      original2: String,
      expected2: String,
      options: Options
  ): Unit = {
    val original = original2.replace("'''", "\"\"\"")
    val expected = expected2.replace("'''", "\"\"\"")
    val testName = logger.revealWhitespace(original)
    test(testName) {
      val root = ScalaPrinter.getRoot(original, options)
      val obtained =
        ScalaPrinter.printTree(root, options).render(options.maxColumn)
      val root2 = ScalaPrinter.getRoot(obtained, options)
      isSameTree(testName, root, root2) match {
        case Left(astDiff) =>
          fail("AST changed!\n" + astDiff)
        case Right(()) =>
          assertNoDiff(obtained, expected)
          val obtained2 =
            ScalaPrinter.printTree(root2, options).render(options.maxColumn)
          assertNoDiff(obtained, obtained2, "Idempotency violated!")
      }
    }
  }

  def getDiff(filename: String, tree1: Tree, tree2: Tree): String = {
    def unified(a: String, b: String) = {
      DiffUtils.unifiedDiff(
        filename,
        filename + "-formatted",
        a.lines.toList,
        b.lines.toList,
        3
      )
    }
    val x = ScalametaInternal.resetOrigin(tree1).syntax
    val y = ScalametaInternal.resetOrigin(tree2).syntax
    val result = unified(x, y)
    result
  }
}
