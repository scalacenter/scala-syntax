package scala.meta.internal.prettyprinters

import scala.meta.internal.prettyprinters.{ScalaToken => S}

import scala.meta.Tree

import scala.meta.internal.paiges.Doc

trait SyntacticGroupOps extends WithPrinter {
  def wrapParens(doc: Doc): Doc = wrapParens(S.`(`, doc, S.`)`)
  def wrapParens(`(`: Doc, doc: Doc, `)`: Doc): Doc = `(` + doc + `)`

  implicit class XtensionSyntacticGroup(val leftGroup: SyntacticGroup) {
    def wrap(tree: Tree): Doc = {
      wrap(tree, Side.Left)
    }
    def wrap(tree: Tree, side: Side): Doc = {
      wrap(None, tree, None, side)
    }
    def wrap(`(`: Doc, tree: Tree, `)`: Doc, side: Side): Doc = {
      wrap(Some(`(`), tree, Some(`)`), side)
    }
    private def wrap(
        `(`: Option[Doc],
        tree: Tree,
        `)`: Option[Doc],
        side: Side
    ): Doc = {
      wrap0(tree, `(`, print(tree), `)`, side)
    }
    def wrap0(tree: Tree, doc: Doc): Doc = {
      wrap0(tree, doc, Side.Left)
    }
    def wrap0(tree: Tree, doc: Doc, side: Side): Doc = {
      wrap0(tree, None, doc, None, side)
    }
    private def wrap0(
        tree: Tree,
        `(`: Option[Doc],
        doc: Doc,
        `)`: Option[Doc],
        side: Side
    ): Doc = {
      val rightGroup = TreeSyntacticGroup(tree)
      wrap1(rightGroup, `(`, doc, `)`, side)
    }
    def wrap1(
        rightGroup: SyntacticGroup,
        doc: Doc
    ): Doc = {
      wrap1(rightGroup, doc, Side.Left)
    }
    def wrap1(
        rightGroup: SyntacticGroup,
        doc: Doc,
        side: Side
    ): Doc = {
      wrap1(rightGroup, None, doc, None, side)
    }

    private def wrap1(
        rightGroup: SyntacticGroup,
        `(`: Option[Doc],
        doc: Doc,
        `)`: Option[Doc],
        side: Side
    ): Doc = {
      if (TreeOps.groupNeedsParenthesis(leftGroup, rightGroup, side)) {
        wrapParens(`(`.getOrElse(S.`(`), doc, `)`.getOrElse(S.`)`))
      } else {
        `(`.getOrElse(Doc.empty) + doc + `)`.getOrElse(Doc.empty)
      }
    }
  }
}
