package scala.meta.internal.prettyprinters

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger

import me.tongfei.progressbar.{ProgressBar => PB, ProgressBarStyle}

import org.scalameta.logger

import scala.meta._
import scala.meta.testkit.Corpus

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

sealed trait PropertyResult
case object Success extends PropertyResult
case class Failure(explanation: String) extends PropertyResult

abstract class PropertyTest(name: String) extends BaseScalaPrinterTest {

  def check(file: Input.File, relativePath: String): PropertyResult

  private val ignoreRegressions = false

  private val failed = TrieMap.empty[File, Boolean]
  private val regressions = TrieMap.empty[File, Boolean]
  private val nl = "\n"
  private val prefix = "target/repos/"

  private val coverageFile = Paths.get(s"coverage-${name}.txt")
  private val todoFile = Paths.get(s"todo-${name}.diff")

  if (Files.exists(todoFile)) {
    Files.delete(todoFile)
  }

  private val previouslyFailed: Set[File] =
    if (Files.exists(coverageFile)) {
      val input = new String(Files.readAllBytes(coverageFile))
      input.split(nl).filterNot(_ == "").map(f => new File(prefix + f)).toSet
    } else {
      Set()
    }

  private def fileList(in: TrieMap[File, Boolean], sep: String): String =
    in.keys
      .map(_.toString.drop(prefix.size))
      .toList
      .sorted
      .mkString("", sep, sep)

  test(name) {
    val failureCount = new AtomicInteger(0)
    val successCount = new AtomicInteger(0)

    val corpus = Corpus
      .files(Corpus.fastparse)
      .toBuffer
      .par

    val progress = new PB(
      "Formatting",
      corpus.size,
      1000,
      System.out,
      ProgressBarStyle.UNICODE_BLOCK
    )
    progress.start()

    SyntaxAnalysis.run[Unit](corpus) { file =>
      try {
        val jFile = file.jFile
        val input = Input.File(jFile, StandardCharsets.UTF_8)
        val relativePath = jFile.toString

        check(input, relativePath) match {
          case Success => successCount.incrementAndGet()
          case Failure(explanation) => {
            val failures = failureCount.incrementAndGet()
            failed += jFile -> true

            // if (failures < 100) {
            //   logger.elem(explanation)
            // }

            if (!previouslyFailed.contains(file.jFile)) {
              if (!ignoreRegressions) {
                regressions += file.jFile -> true
                print(Console.RED)
                println("*************************")
                println("Regression: " + file.jFile)
                println("*************************")
                print(Console.RESET)
              }
            } else {
              Files.write(
                todoFile,
                (explanation + nl).toString.getBytes("utf-8"),
                StandardOpenOption.CREATE,
                StandardOpenOption.APPEND
              )
            }
          }
        }
      } catch {
        case NonFatal(_) => ()
      }

      progress.synchronized {
        progress.step()

        val currentFailures = failureCount.get
        val currentSuccess = successCount.get

        val progressMessage =
          if (currentFailures > 100) {
            val rate = (currentSuccess.toDouble / (currentFailures + currentSuccess).toDouble) * 100.0
            f"Success: ${rate}%.2f%%"
          } else {
            s"Failures: $currentFailures"
          }

        progress.setExtraMessage(progressMessage)
      }
    }

    progress.stop()

    if (regressions.isEmpty || ignoreRegressions) {
      Files.write(
        coverageFile,
        fileList(failed, nl).getBytes("utf-8"),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

    if (regressions.nonEmpty && !ignoreRegressions) {
      val sep = nl + "  "
      val regressionList = fileList(regressions, sep)
      sys.error("Regressions:" + sep + regressionList)
    }
  }
}
