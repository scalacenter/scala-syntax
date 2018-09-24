package scala.meta.internal.prettyprinters

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger

import me.tongfei.progressbar.{ProgressBar => PB, ProgressBarStyle}

import org.scalameta.logger

import scala.meta._
import scala.meta.testkit.Corpus
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

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
  private val regressionFile = Paths.get(s"regressions-${name}.txt")
  private val todoFile = Paths.get(s"todo-${name}.diff")

  if (Files.exists(todoFile)) {
    Files.delete(todoFile)
  }

  if (Files.exists(regressionFile)) {
    Files.delete(regressionFile)
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

  val corpusExclude = Set(
    "target/repos/shapeless/core/src/test/scala/shapeless/hlist.scala" // fail to parse
  )

  val corpusFiles =
    Corpus.fastparse.copy(
      filter =
        file => Corpus.fastparse.filter(file) && !corpusExclude.contains(file)
    )

  test(name) {
    val failureCount = new AtomicInteger(0)
    val successCount = new AtomicInteger(0)

    val corpus = Corpus
      .files(corpusFiles)
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
      val regressionFilepath = file.jFile.toString.stripPrefix(prefix)
      try {
        val jFile = file.jFile
        val input = Input.File(jFile, StandardCharsets.UTF_8)
        val relativePath = jFile.toString

        check(input, relativePath) match {
          case Success => successCount.incrementAndGet()
          case Failure(explanation) => {
            val failures = failureCount.incrementAndGet()
            failed += jFile -> true

            if (!previouslyFailed.contains(file.jFile)) {
              if (!ignoreRegressions) {
                regressions += file.jFile -> true

                Files.write(
                  regressionFile,
                  (regressionFilepath + nl).getBytes("utf-8"),
                  StandardOpenOption.CREATE,
                  StandardOpenOption.APPEND
                )

                print(Console.RED)
                println("*************************")
                println("Regression: " + regressionFilepath)
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
        case e: ParseException => ()
        case e: TokenizeException => ()
        case XmlSpliceEndNotFound => ()

        case NonFatal(e) =>
          println("****")
          println(regressionFilepath)
          println(e.getClass)
          e.printStackTrace()
          println("****")
      }

      progress.synchronized {
        progress.step()

        val currentFailures = failureCount.get
        val currentSuccess = successCount.get
        val total = currentFailures + currentSuccess

        val progressMessage =
          if (currentFailures > 100) {
            val rate = (currentSuccess.toDouble / total.toDouble) * 100.0
            f"${rate}%.2f%%($currentFailures)"
          } else {
            s"$currentFailures"
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
