package org.scalafmt.tests

import scala.meta.Tree
import scala.meta.dialects
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

  object normalizeTransform extends Transformer {
    import scala.meta._

    val transform: PartialFunction[Tree, Tree] = {
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
      var newTree = tree
      var i = 0
      val limit = 100
      var again = true
      do {
        transform.lift(newTree) match {
          case Some(t) =>
            newTree = t
            again = false
          case None => 
            again = true
        }
        i += 1
      } while(again && i < limit)

      super.apply(newTree)
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

  def check(
      original2: String,
      expected2: String,
      options: Options
  ): Unit = {
    val original = original2.replace("'''", "\"\"\"")
    val expected = expected2.replace("'''", "\"\"\"")
    val testName = logger.revealWhitespace(original)
    test(testName) {
      val root = TreeDocOps.getRoot(original, options)
      val obtained =
        TreeDocOps.printTree(root, options).render(options.maxColumn)
      val root2 = TreeDocOps.getRoot(obtained, options)
      isSameTree(testName, root, root2) match {
        case Left(astDiff) =>
          sys.error("AST changed!\n" + astDiff)
        case Right(()) =>
          assertNoDiff(obtained, expected)
          val obtained2 =
            TreeDocOps.printTree(root2, options).render(options.maxColumn)
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
