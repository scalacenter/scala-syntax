package org.scalafmt.internal

import org.scalafmt.internal.{AssociatedTrivias => Trivias}
import org.scalafmt.internal.TreePrinter.print
import org.scalafmt.internal.ScalaToken._

import scala.meta.Tree
import scala.meta.internal.fmt.SyntacticGroup

import org.typelevel.paiges.Doc

object SyntacticGroupOps {
  def wrapParens(doc: Doc): Doc = `(` + doc + `)`

  implicit class XtensionSyntacticGroup(val leftGroup: SyntacticGroup)
      extends AnyVal {
    def wrap(tree: Tree, side: Side = Side.Left)(
        implicit trivias: Trivias
    ): Doc = {
      wrap0(tree, print(tree), side)
    }
    def wrap0(tree: Tree, doc: Doc, side: Side = Side.Left)(
        implicit trivias: Trivias
    ): Doc = {
      val rightGroup = TreeSyntacticGroup(tree)
      wrap1(rightGroup, doc, side)
    }
    def wrap1(
        rightGroup: SyntacticGroup,
        doc: Doc,
        side: Side = Side.Left
    )(implicit trivias: Trivias): Doc = {
      if (TreeOps.groupNeedsParenthesis(leftGroup, rightGroup, side))
        wrapParens(doc)
      else doc
    }
  }
}
