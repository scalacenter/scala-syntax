package org.scalafmt.internal

import scala.meta._
import org.scalafmt.Options

import scala.meta.testkit.StructurallyEqual

object CorrectnessTest extends PropertyTest("correctness") {
  private val options = Options.default
  def check(file: Input.File): Either[String, Unit] = {
    val originalTree = file.parse[Source].get
    val formatted =
      TreeDocOps.printTree(originalTree, options).render(options.maxColumn)
    val formattedTree = formatted.parse[Source].get
    val normalizedOriginalTree = normalize(originalTree)
    val normalizeFormattedTree = normalize(formattedTree)

    if (StructurallyEqual(normalizedOriginalTree, normalizeFormattedTree).isRight) {
      val diff = getDiff(
        file.path.toString,
        normalizedOriginalTree,
        normalizeFormattedTree
      )
      if (diff.nonEmpty) {
        Left(diff)
      } else {
        Right(())
      }
    } else {
      Right(())
    }
  }
}
