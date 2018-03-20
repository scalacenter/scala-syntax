package org.scalafmt.internal

import scala.meta._
import scala.meta.tokens.Token

object PreserveCommentsTest extends PropertyTest("comments") {

  def extractComments(tree: Tree): String = {
    val nl = "\n"
    val sep = "--------------------------------------"
    val comments =
      tree.tokens.collect {
        case Token.Comment(content) => content
      }
    comments.mkString("", nl + sep + nl, nl)
  }

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
