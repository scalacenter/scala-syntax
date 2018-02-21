package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.testkit.Corpus
import scala.meta.testkit.CorpusFile
import scala.meta.testkit.StructurallyEqual
import scala.util.control.NonFatal
import org.scalafmt.Format
import org.scalafmt.Options
import org.scalafmt.tests.BaseScalaPrinterTest
import org.scalameta.logger
import org.scalatest.Ignore

import me.tongfei.progressbar.{ProgressBar => PB, ProgressBarStyle}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap

import java.nio.file._
import java.io.File

// Comment out to run these tests, currently it fails with output
// https://gist.github.com/olafurpg/ea44f3567d4117e53ca818b1911f9be9
@Ignore
/** Tests that running printer twice always yields the same results */
object IdempotencyPropertyTest extends BaseScalaPrinterTest {

  val coverageFile = Paths.get("coverage.txt")
  val failed = TrieMap.empty[File, Boolean]
  val regressions = TrieMap.empty[File, Boolean]

  val nl = "\n"

  val prefix = "target/repos/"

  val previouslyFailed: Set[File] =
    if (Files.exists(coverageFile)) {
      val input = new String(Files.readAllBytes(coverageFile))
      input.split(nl).map(f => new File(prefix + f)).toSet
    } else {
      Set()
    }

  val failureCount = new AtomicInteger(0)
  val successCount = new AtomicInteger(0)

  def isOk(file: CorpusFile): Boolean =
    !List(
      "scalaxy/debug/package.scala" // https://github.com/scalameta/scalameta/issues/1136
    ).exists(file.filename.contains)

  // first step towards idempotency is to print out the identical AST
  // as the input source code.
  test("AST is unchanged") {
    val corpus = Corpus
      .files(Corpus.fastparse)
      .filter(f => isOk(f))
      .toBuffer
      // .reverse
      .par

    val progress = new PB(
      "Formatting",
      corpus.size,
      1000,
      System.out,
      ProgressBarStyle.UNICODE_BLOCK
    )
    progress.start()

    def currentRate: String = {
      val currentFailures = failureCount.get
      val currentSuccess = successCount.get
      val rate = (currentSuccess.toDouble / (currentFailures + currentSuccess).toDouble) * 100.0
      f"$rate%.2f%%"
    }

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
          if (StructurallyEqual(treeNorm, tree2Norm).isRight) {
            successCount.incrementAndGet()
            Nil
          } else {
            val diff = getDiff(file.jFile.getAbsolutePath, treeNorm, tree2Norm)
            if (diff.nonEmpty) {
              logger.elem(diff)
              failureCount.incrementAndGet()
              failed += file.jFile -> true

              if (!previouslyFailed.contains(file.jFile)) {
                regressions += file.jFile -> true
                println("Regression: " + file.jFile)
              }
              () :: Nil
            } else {
              successCount.incrementAndGet()
              Nil
            }
          }
        } catch {
          case NonFatal(e) =>
            val st = e.getStackTrace
              .filter(_.getClassName.startsWith("org.scalafmt"))
              .take(10)
            e.setStackTrace(st)
            // e.printStackTrace()
            failed += file.jFile -> true
            () :: Nil
        }

      progress.synchronized {
        progress.step()

        val currentFailures = failureCount.get
        val currentSuccess = successCount.get

        val rate = (currentSuccess.toDouble / (currentFailures + currentSuccess).toDouble) * 100.0
        progress.setExtraMessage(s"Success: $currentRate")
      }

      result
    }

    progress.stop()

    def fileList(in: TrieMap[File, Boolean], sep: String): String =
      in.keys.map(_.toString.drop(prefix.size)).toList.sorted.mkString(sep)

    if (regressions.isEmpty) {
      Files.write(
        coverageFile,
        fileList(failed, nl).getBytes("utf-8"),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

    println(s"Success Rate: $currentRate")

    if (regressions.nonEmpty) {
      val sep = nl + "  "
      val regressionList = fileList(regressions, sep)
      sys.error("Regressions:" + sep + regressionList)
    }
  }
}
