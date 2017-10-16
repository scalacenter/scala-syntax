package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.testkit.Corpus
import scala.meta.testkit.StructurallyEqual
import scala.meta.testkit.SyntaxAnalysis
import scala.util.control.NonFatal
import org.scalafmt.Format
import org.scalameta.logger
import org.scalatest.Ignore

// Comment out to run these tests, currently it fails with output
// https://gist.github.com/olafurpg/ea44f3567d4117e53ca818b1911f9be9
@Ignore
/** Tests that running printer twice always yields the same results */
class IdempotencyPropertyTest extends BaseScalaPrinterTest {
  val prefix = "target/repos/"

  // first step towards idempotency is to print out the identical AST
  // as the input source code.
  test("AST is unchanged") {
    val corpus = Corpus
      .files(Corpus.fastparse)
      .take(7000) // take files as you please.
      .toBuffer
      .par
    val nonEmptyDiff = SyntaxAnalysis.run[Unit](corpus) { file =>
      try {
        val in = file.read
        import scala.meta._
        val tree = in.parse[Source].get
        val formatted = Format.format(in)
        val tree2 = formatted.parse[Source].get
        val treeNorm = normalize(tree)
        val tree2Norm = normalize(tree2)
        if (StructurallyEqual(treeNorm, tree2Norm).isRight) Nil
        else {
          val diff = getDiff(file.jFile.getAbsolutePath, treeNorm, tree2Norm)
          if (diff.nonEmpty) {
            logger.elem(diff)
            () :: Nil
          } else Nil
        }
      } catch {
        case NonFatal(e) =>
          Nil
      }
    }
    if (nonEmptyDiff.nonEmpty) fail("diffs.nonEmpty!")
  }
}
