package org.scalafmt.tests

import scala.meta._
import scala.meta.internal.ScalametaInternal
import scala.meta.parsers.Parse
import scala.meta.testkit.AnyDiff
import scala.meta.testkit.StructurallyEqual
import scala.meta.transversers.Transformer

import scalafix.diff.DiffUtils

import org.scalafmt.InternalOptions
import org.scalafmt.Options
import org.scalameta.logger
import org.scalafmt.internal.TreeDocOps

abstract class BaseScalaPrinterTest extends DiffSuite {

  val defaultOptions: InternalOptions = InternalOptions(100).copy(
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

  def checkCaseStructural(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    checkStructural(original, options.copy(parser = Parse.parseCase))
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

  def checkSourceStructural(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    checkStructural(
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

  def checkStructural(
      original: String,
      options: Options = defaultOptions
  ): Unit = {
    checkStructural(original, original, options)
  }

  def check(original: String, expected: String): Unit = {
    check(original, expected, defaultOptions)
  }

  object normalizeTransform extends Transformer {
    import scala.meta._

    val transform: PartialFunction[Tree, Tree] = {
      case Source(List(t)) => t
      case Term.Block(a :: Nil) if !a.is[Defn] => a
      case Term.ApplyInfix(lhs, op, targs, args) =>
        if (targs.isEmpty) q"$lhs.$op(..$args)"
        else q"$lhs.$op[..$targs](..$args)"
      case Term.Block((f @ Term.PartialFunction(_)) :: Nil) =>
        f
      case f @ Term.Function(_, Term.Block(_ :: _ :: _))
          if !f.parent.exists(_.is[Term.Block]) =>
        Term.Block(f :: Nil)
    }

    override def apply(tree: Tree): Tree = {
      super.apply(transform.lift(tree).map(this.apply).getOrElse(tree))
    }
  }

  def normalize(tree: Tree): Tree = {
    val input = tree.tokens.head.input match {
      case scala.meta.Input.VirtualFile(path, _) => path
      case _ => "<input>"
    }
    try {
      normalizeTransform(tree)
    } catch {
      case e: UnsupportedOperationException =>
        throw new IllegalArgumentException(s"Failed to transform $input", e)
    }
  }

  def isStructurallyEqual(a: Tree, b: Tree): Either[AnyDiff, Unit] =
    StructurallyEqual(normalize(a), normalize(b))

  def isSameTree(filename: String, a: Tree, b: Tree): Either[String, Unit] = {
    isStructurallyEqual(a, b).left.map(_ => getDiff(filename, a, b))
  }

  def checkStructural(
      original2: String,
      expected2: String,
      options: Options
  ): Unit = {
    checkFromString(
      original2,
      expected2,
      options,
      structuralOnly = true
    )
  }

  def check(
      original2: String,
      expected2: String,
      options: Options
  ): Unit = {
    checkFromString(
      original2,
      expected2,
      options,
      structuralOnly = false
    )
  }

  private def checkFromString(
      original2: String,
      expected2: String,
      options: Options,
      structuralOnly: Boolean
  ): Unit = {
    val original = original2.replace("'''", "\"\"\"")
    val expected = expected2.replace("'''", "\"\"\"")
    val testName = logger.revealWhitespace(original)
    test(testName) {
      val root = TreeDocOps.getRoot(original, options)
      val obtained = printTree(root, options)
      val root2 = TreeDocOps.getRoot(obtained, options)
      isSameTree(testName, root, root2) match {
        case Left(astDiff) =>
          sys.error(
            s"""|## AST changed ##
                |- diff -
                |$astDiff
                |
                |- obtained -
                |$obtained
                |
                |- expected -
                |$expected
                |
                |---------------------------------""".stripMargin
          )

        case Right(()) =>
          if (!structuralOnly) {
            assertNoDiff(obtained, expected)
            val obtained2 = printTree(root2, options)
            assertNoDiff(obtained, obtained2, "Idempotency violated!")
          }
      }
    }
  }

  def check(input: Input): Unit = {
    checkTreeSource(input.parse[Source].get)
  }

  def checkTreeSource(root: Tree): Unit = {
    val testName = root.syntax
    val options = defaultOptions.copy(
      parser = Parse.parseSource,
      dialect = dialects.Scala212
    )

    test(testName) {
      val obtained = printTree(root, options)
      val root2 = TreeDocOps.getRoot(obtained, options).children.head
      isSameTree(testName, root, root2) match {
        case Left(astDiff) =>
          sys.error(
            s"""|## AST changed ##
                |- diff -
                |$astDiff
                |
                |- obtained -
                |${root2.syntax}
                |
                |${root2.structure} 
                |
                |- expected -
                |${root2.syntax}
                |
                |${root.structure}
                |
                |---------------------------------""".stripMargin
          )
        case Right(()) => ()
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

  def printTree(root: Tree, options: Options = defaultOptions): String = {
    TreeDocOps.printTree(root, options).render(options.maxColumn)
  }

  def check(tree: Tree, expected: String): Unit = {
    assertNoDiff(printTree(tree), expected)
  }
}
