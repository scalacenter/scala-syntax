package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.testkit.Corpus
import scala.meta.testkit.CorpusFile
import scala.meta.testkit.StructurallyEqual
import scala.meta.testkit.SyntaxAnalysis
import scala.util.control.NonFatal
import org.scalafmt.Format
import org.scalafmt.Options
import org.scalafmt.tests.BaseScalaPrinterTest
import org.scalameta.logger
import org.scalatest.Ignore

import me.tongfei.progressbar.{ProgressBar => PB, ProgressBarStyle}
import java.util.concurrent.atomic.AtomicInteger

// Comment out to run these tests, currently it fails with output
// https://gist.github.com/olafurpg/ea44f3567d4117e53ca818b1911f9be9
@Ignore
/** Tests that running printer twice always yields the same results */
object IdempotencyPropertyTest extends BaseScalaPrinterTest {
  val prefix = "target/repos/"

  val failures = new AtomicInteger(0)
  val success = new AtomicInteger(0)

  def isOk(file: CorpusFile): Boolean =
    !List(
      "scalaxy/debug/package.scala" // https://github.com/scalameta/scalameta/issues/1136
    ).exists(file.filename.contains)

  // first step towards idempotency is to print out the identical AST
  // as the input source code.
  test("AST is unchanged") {
    val corpus = Corpus
      .files(Corpus.fastparse)
      // .take(7000) // take files as you please.
      .filter(f => isOk(f))
      .toBuffer
      .par

    val progress = new PB("Formatting", corpus.size, 1000, System.out, ProgressBarStyle.UNICODE_BLOCK)
    progress.start()

    val options = Options.default
    val nonEmptyDiff = SyntaxAnalysis.run[Unit](corpus) { file =>
      val result = 
        try {
          val in = file.read
          import scala.meta._
          def input(str: String) =
            Input.VirtualFile(file.jFile.getAbsolutePath, str)
          val tree = input(in).parse[Source].get
          val formatted =
            TreeDocOps.printTree(tree, options).render(options.maxColumn)
          val tree2 = input(formatted).parse[Source].get
          val treeNorm = normalize(tree)
          val tree2Norm = normalize(tree2)
          val ok = 
            if (StructurallyEqual(treeNorm, tree2Norm).isRight) Nil
            else {
              val diff = getDiff(file.jFile.getAbsolutePath, treeNorm, tree2Norm)
              if (diff.nonEmpty) {
                logger.elem(diff)
                () :: Nil
              } else Nil
            }
          success.incrementAndGet()

          ok
        } catch {
          case NonFatal(e) =>
            val st = e.getStackTrace
              .filter(_.getClassName.startsWith("org.scalafmt"))
              .take(10)
            e.setStackTrace(st)
  //          e.printStackTrace()
            failures.incrementAndGet()
            () :: Nil
        }

      progress.synchronized {
        progress.step()

        val currentFailures = failures.get
        val currentSuccess = success.get
        val rate = (currentSuccess.toDouble / (currentFailures + currentSuccess).toDouble) * 100.0
        progress.setExtraMessage(f"Success Rate: ${rate}%.0f%%")
      }

      result
    }

    progress.stop()

    if (nonEmptyDiff.nonEmpty) {
      val percentage = 100.0 - (nonEmptyDiff.size.toDouble / corpus.size.toDouble * 100)
      sys.error(f"Success: $percentage%.2f")
    }
  }
}
