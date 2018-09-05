package scala.meta.internal.prettyprinters

import scala.meta._
import scala.meta.internal.ScalametaInternal
import scala.meta.parsers.Parse
import scala.meta.testkit.AnyDiff
import scala.meta.testkit.StructurallyEqual
import scala.meta.transversers.Transformer
import org.scalameta.logger

import scalafix.diff.DiffUtils

import scala.util.control.NonFatal

abstract class BaseScalaPrinterTest extends DiffSuite {

  val defaultOptions: InternalOptions = InternalOptions(100).copy(
    dialect = dialects.Sbt1.copy(
      allowTypeLambdas = true,
      allowAndTypes = true,
      allowImplicitFunctionTypes = true
    ),
    parser = Parse.parseStat
  )

  val dotty = defaultOptions.copy(dialect = dialects.Dotty)

  private val options = Options.default

  def prettyPrint(tree: Tree): String =
    TreePrinter.printTree(tree, options).render(options.maxColumn)

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
      original: String,
      expected: String,
      options: Options
  ): Unit = {
    checkFromString(
      original,
      expected,
      options,
      structuralOnly = true
    )
  }

  def check(
      original: String,
      expected: String,
      options: Options
  ): Unit = {
    checkFromString(
      original,
      expected,
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
    val original = original2.stripMargin.replace("'''", "\"\"\"")
    val expected = expected2.stripMargin.replace("'''", "\"\"\"")
    val testName = logger.revealWhitespace(original)
    test(testName) {
      val originalTree = TreePrinter.getRoot(original, options)
      val formattedCode = printTree(originalTree, options)
      val formattedTree =
        try {
          TreePrinter.getRoot(formattedCode, options)
        } catch {
          case NonFatal(e) => {
            throw new Exception(
              "formatted code does not parse: \n\n" + formattedCode + "\n\n\n"
            )
          }
        }

      isSameTree(testName, originalTree, formattedTree) match {
        case Left(astDiff) =>
          sys.error(
            s"""|## AST changed ##
                |- diff -
                |$astDiff
                |
                |- formatted -
                |$formattedCode
                |
                |- expected -
                |$expected
                |
                |---------------------------------""".stripMargin
          )

        case Right(()) =>
          if (!structuralOnly) {
            assertNoDiff(formattedCode, expected)
            val reformattedCode = printTree(formattedTree, options)
            assertNoDiff(
              formattedCode,
              reformattedCode,
              "Idempotency violated!"
            )
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
      val root2 = TreePrinter.getRoot(obtained, options).children.head
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

  def unified(filename: String, original: String, modified: String): String = {
    DiffUtils.unifiedDiff(
      filename,
      filename + "-formatted",
      original.lines.toList,
      modified.lines.toList,
      3
    )
  }
  def getDiff(filename: String, original: Tree, modified: Tree): String = {
    val originalCode = ScalametaInternal.resetOrigin(original).syntax
    val modifiedCode = ScalametaInternal.resetOrigin(modified).syntax
    val result = unified(filename, originalCode, modifiedCode)
    result
  }

  def printTree(root: Tree, options: Options = defaultOptions): String = {
    TreePrinter.printTree(root, options).render(options.maxColumn)
  }

  def check(tree: Tree, expected: String): Unit = {
    assertNoDiff(printTree(tree), expected)
  }
}
