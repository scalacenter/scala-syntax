package scala.meta.internal.format

import scala.meta.Name
import scala.meta.Tree

object FormatTree {
  case class PatName(value: String) extends Name {
    override def children: List[Tree] = Nil
    override def productFields: List[String] = "value" :: Nil
    def privateCopy(
        prototype: scala.meta.Tree,
        parent: scala.meta.Tree,
        destination: String,
        origin: scala.meta.internal.trees.Origin
    ): scala.meta.Tree = ???
    def privateOrigin: scala.meta.internal.trees.Origin = ???
    def privateParent: scala.meta.Tree = ???
    def privatePrototype: scala.meta.Tree = ???
  }
}
