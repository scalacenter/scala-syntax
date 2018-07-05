package org.scalafmt.internal

import org.scalafmt.internal.ScalaToken._

import scala.meta.Tree
import scala.meta.internal.fmt.SyntacticGroup

import scala.meta.internal.paiges.Doc

trait SyntacticGroupOps extends WithPrinter {
  def wrapParens(doc: Doc): Doc = `(` + doc + `)`

  implicit class XtensionSyntacticGroup(val leftGroup: SyntacticGroup) {
    def wrap(tree: Tree, side: Side = Side.Left): Doc = {
      wrap0(tree, print(tree), side)
    }
    def wrap0(tree: Tree, doc: Doc, side: Side = Side.Left): Doc = {
      val rightGroup = TreeSyntacticGroup(tree)
      wrap1(rightGroup, doc, side)
    }
    def wrap1(
        rightGroup: SyntacticGroup,
        doc: Doc,
        side: Side = Side.Left
    ): Doc = {
      if (TreeOps.groupNeedsParenthesis(leftGroup, rightGroup, side))
        wrapParens(doc)
      else doc
    }
  }
}
