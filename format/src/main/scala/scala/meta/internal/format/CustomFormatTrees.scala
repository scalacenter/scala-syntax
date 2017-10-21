package scala.meta.internal.format

import scala.meta.Name
import scala.meta.Tree

/**
  * Custom scala.meta.Tree nodes that
  */
object CustomFormatTrees {
  sealed trait CustomTree extends Tree {
    private def unsupported = throw new UnsupportedOperationException(
      s"Custom scala-format tree '$productPrefix' does not support this operation."
    )
    def privateCopy(
        prototype: scala.meta.Tree,
        parent: scala.meta.Tree,
        destination: String,
        origin: scala.meta.internal.trees.Origin
    ): scala.meta.Tree = unsupported
    def privateOrigin: scala.meta.internal.trees.Origin = unsupported
    def privateParent: scala.meta.Tree = unsupported
    def privatePrototype: scala.meta.Tree = unsupported
  }

  /**
   * PatName is a backticked name in a pattern.
   *
   * Example: case `a` =>
   *
   * By default, Term.Name represents this tree node in Scalameta when it's
   * not the child of a Pat.Var. This greatly complicates print(Term.Name)
   * since it may have to be backticked or not. PatName avoids the need
   * to keep track of a custom context.
   */
  case class PatName(value: String) extends CustomTree with Name {
    override def children: List[Tree] = Nil
    override def productFields: List[String] = "value" :: Nil
  }
}
