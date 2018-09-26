package scala.meta.internal.prettyprinters

import scala.meta._

object CommentsSuite extends BaseScalaPrinterTest {
  checkComments("preserve comments 2", resource("comments2.scala"))
  checkComments("preserve comments", resource("comments.scala"))
  checkComments("preserve scaladoc", resource("scaladoc.scala"))
}
