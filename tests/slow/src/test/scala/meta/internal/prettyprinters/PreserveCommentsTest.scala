package scala.meta.internal.prettyprinters

import scala.meta._
import scala.meta.tokens.Token

object PreserveCommentsTest extends PropertyTest("comments") {

  def noDiff(
      relativePath: String,
      originalComments: String,
      formattedComments: String
  ): PropertyResult = {
    if (originalComments != formattedComments) {
      val maxSizeForDiff = 1000
      if (originalComments.size < maxSizeForDiff && formattedComments.size < maxSizeForDiff) {
        val diff = unified(relativePath, originalComments, formattedComments)
        if (diff.isEmpty) Success
        else Failure(diff)
      } else Failure("-- no diff --")
    } else Success
  }

  def check(file: Input.File, relativePath: String): PropertyResult = {
    val originalTree = file.parse[Source].get
    val originalComments = extractComments(originalTree)

    val formattedComments = {
      val formatted = prettyPrint(originalTree)
      val formattedTree = formatted.parse[Source].get
      extractComments(formattedTree)
    }

    noDiff(relativePath, originalComments, formattedComments)
  }
}
