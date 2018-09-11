package scala.meta.internal.prettyprinters

import scala.meta._

object CommentsSuite2 extends BaseScalaPrinterTest {
  checkComments("preserve comments", resource("comments.scala"))
  checkComments("preserve scaladoc", resource("scaladoc.scala"))
}
