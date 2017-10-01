package scala.meta.internal

import scala.meta.Tree
import scala.meta.internal.trees.Origin

object ScalametaInternal {
  def resetOrigin(tree: Tree): Tree = tree.withOrigin(Origin.None)
}
