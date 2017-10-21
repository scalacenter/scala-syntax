package scala.meta.internal.format

import java.io.InputStream
import java.nio.charset.StandardCharsets
import scala.meta._
import scala.meta.internal.tokens.TokenStreamPosition
import scala.meta.internal.trees.Origin
import org.typelevel.paiges.Doc

case class Comments(leading: List[String], trailing: List[String])
    extends InputStream {
  def join(ss: List[String]): Doc = Doc.intercalate(Doc.empty, ss.map(Doc.text))
  def wrap(doc: Doc): Doc = join(leading) + doc + join(trailing)
  override def read(): Int = 1
}

object Comments {
  val default = Comments(Nil, Nil)
  def doc(tree: Tree, print: Doc): Doc = Comments(tree).wrap(print)
  def apply(tree: Tree): Comments = tree.origin match {
    case Origin.Parsed(Input.Stream(c: Comments, _), _, _) => c
    case _ => default
  }
  implicit class XtensionTreeComments[T <: Tree](val tree: T) extends AnyVal {

    /**
     * Attach a leading comment to this tree node.
     *
     * {{{
     * > import Comments._
     * > q"def foo = 2".withLeadingComment("/** Returns 2 */")
     * res0: String = "
     * /** Returns 2 */
     * def foo = 2
     * "
     * }}}
     *
     * @param comment the string is formatted raw, except indentation is adapted
     * to the indentation of the tree node.
     */
    def withLeadingComment(comment: String): T =
      withComments(x => x.copy(leading = comment :: x.leading))

    /**
     * Attach a trailing comment to this tree node.
     *
     * {{{
     * > import Comments._
     * > q"def foo = 2".withTrailingComment("FIXME")
     * res0: String = "
     * def foo = 2 // FIXME
     * "
     * }}}
     *
     * @param comment the string is formatted raw, except indentation is adapted
     * to the indentation of the tree node.
     */
    def withTrailingComment(comment: String): T =
      withComments(x => x.copy(trailing = comment :: x.trailing))
    private def withComments(f: Comments => Comments): T = tree.withOrigin(
      tree.origin match {
        case o @ Origin.Parsed(
              i @ Input.Stream(c @ Comments(l, t), _),
              _,
              _
            ) =>
          o.copy(input = i.copy(stream = f(c)))
        case _ =>
          Origin.Parsed(
            Input.Stream(f(default), StandardCharsets.UTF_8),
            dialects.Scala212,
            TokenStreamPosition(-1, -1)
          )
      }
    )
  }
  val input = Input.VirtualFile("SyntheticComments", "")
}
