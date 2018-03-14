package org.scalafmt.internal

import org.scalafmt.Options
import scala.meta.{Input, Tree}
import org.typelevel.paiges.Doc

object TreeDocOps {
  import TreePrinter._

  def getRoot(input: String, options: Options): Tree = {
    getRoot(Input.String(input), options)
  }

  def getRoot(input: Input, options: Options): Tree = {
    options.parser.apply(input, options.dialect).get
  }

  def printInput(input: Input, options: Options): Doc = {
    printTree(getRoot(input, options), options)
  }

  def printTree(root: Tree, options: Options): Doc = {
    print(root)
  }
}