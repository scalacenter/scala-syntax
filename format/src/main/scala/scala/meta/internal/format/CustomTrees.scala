package scala.meta.internal.format

import scala.meta.Name
import scala.meta.Tree
import scala.meta.internal.trees.Origin

/**
 * Custom scala.meta.Tree nodes that are only relevant for pretty printing.
 */
object CustomTrees {
  sealed trait CustomTree extends Tree {
    private def unsupported(implicit enclosing: sourcecode.Enclosing) =
      throw new UnsupportedOperationException(
        s"$productPrefix.${enclosing.value.replaceFirst(".*#", "")} is not supported"
      )
    def privateCopy(
        prototype: scala.meta.Tree,
        parent: scala.meta.Tree,
        destination: String,
        origin: scala.meta.internal.trees.Origin
    ): scala.meta.Tree = unsupported
    def privateOrigin: scala.meta.internal.trees.Origin = Origin.None
    def privateParent: scala.meta.Tree = unsupported
    def privatePrototype: scala.meta.Tree = unsupported
  }

  /**
   * PatName is a backticked name in a pattern.
   *
   * Example, `a` from below:
   *   case `a` =>
   *   case A(`a`, b) =>
   *   case `a` :: b =>
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
