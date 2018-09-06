package scala.meta.internal.prettyprinters

import scala.meta._

import scala.meta.testkit.StructurallyEqual

// The ast must stay the same after it's pretty printed
object IdempotentAstTest extends PropertyTest("idempotent-ast") {
  def check(file: Input.File, relativePath: String): PropertyResult = {
    val originalTree = file.parse[Source].get
    val formatted = prettyPrint(originalTree)
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
