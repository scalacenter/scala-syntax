package org.scalafmt.internal

import scala.meta._
import org.scalafmt.Options

import scala.meta.testkit.StructurallyEqual

object IdempotentAstTest extends PropertyTest("idempotent-ast") {
  private val options = Options.default

  // The ast must stay the same after it's pretty printed
  def check(file: Input.File, relativePath: String): PropertyResult = {
    val originalTree = file.parse[Source].get
    val formatted =
      TreeDocOps.printTree(originalTree, options).render(options.maxColumn)
    val formattedTree = formatted.parse[Source].get
    val normalizedOriginalTree = normalize(originalTree)
    val normalizeFormattedTree = normalize(formattedTree)

    if (StructurallyEqual(normalizedOriginalTree, normalizeFormattedTree).isLeft) {
      val diff = getDiff(
        relativePath,
        normalizedOriginalTree,
        normalizeFormattedTree
      )
      if (diff.nonEmpty) {
        Failure(diff)
      } else {
        Success
      }
    } else {
      Success
    }
  }
}
