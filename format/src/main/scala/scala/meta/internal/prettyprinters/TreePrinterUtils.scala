package scala.meta.internal.prettyprinters

import scala.meta._

import scala.meta.internal.paiges.Doc
import scala.meta.internal.paiges.Doc._

trait TreePrinterUtils extends WithPrinter {
  implicit class XtensionTrees(private val trees: List[Tree]) {
    def mkDoc(separators: List[Doc]): Doc = {
      if (separators.nonEmpty) {
        assert(
          trees.size == separators.size + 1 ||
            trees.size == separators.size
        )
      }
      var i = 0
      val t = trees.size - 1
      trees
        .map(print)
        .zipAll(separators, empty, comma + space)
        .foldLeft(empty) {
          case (acc, (term, sep)) => {
            val s = if (i == t) empty else sep
            i += 1
            acc + term + s
          }
        }
    }

    def mkDoc(start: Doc, separators: List[Doc], end: Doc): Doc = {
      start + mkDoc(separators) + end
    }
  }
}
