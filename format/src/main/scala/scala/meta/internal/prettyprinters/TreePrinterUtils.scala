package scala.meta.internal.prettyprinters

import scala.meta._

import scala.meta.internal.paiges.Doc
import scala.meta.internal.paiges.Doc._

trait TreePrinterUtils extends WithPrinter {
  implicit class XtensionTerms(private val terms: List[Term]) {
    def mkDoc(start: Doc, separators: List[Doc], end: Doc): Doc = {
      assert(terms.size == separators.size + 1)
      val commaSeparated =
        terms.map(print).zipAll(separators, empty, empty).foldLeft(empty) {
          case (acc, (term, sep)) => {
            acc + term + sep
          }
        }
      start + commaSeparated + end
    }
  }
}
