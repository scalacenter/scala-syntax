package scala.meta.internal.prettyprinters

import java.io.InputStream
import java.nio.charset.StandardCharsets
import scala.meta._
import scala.meta.internal.tokens.TokenStreamPosition
import scala.meta.internal.trees.Origin
import scala.meta.internal.paiges.Doc

case class Comments(leading: List[String], trailing: List[String])
    extends InputStream {
  def join(ss: List[String]): Doc = Doc.intercalate(Doc.empty, ss.map(Doc.text))
  def wrap(doc: Doc): Doc = join(leading) + doc + join(trailing)
  override def read(): Int = throw new Exception("Cannot read comments stream")
}

object Comments {
  val default = Comments(Nil, Nil)
  def apply(tree: Tree, doc: Doc)(implicit trivia: AssociatedTrivias): Doc = {
    val comments =
      tree.origin match {
        case Origin.Parsed(Input.Stream(c: Comments, _), _, _) => c
        case _ => default
      }

    trivia.wrap(tree, comments.wrap(doc))
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
    def hasTokens: Boolean =
      tree.origin match {
        case o @ Origin.Parsed(Input.Stream(_: Comments, _), _, _) => false
        case _: Origin.Parsed => true
        case Origin.None => false
        case e => sys.error("unsupported origin: " + e)
      }
  }
  val input = Input.VirtualFile("SyntheticComments", "")
}
