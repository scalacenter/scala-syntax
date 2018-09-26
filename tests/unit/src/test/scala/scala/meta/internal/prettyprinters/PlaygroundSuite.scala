package scala.meta.internal.prettyprinters

import scala.meta._
import java.nio.file.Paths

object PlaygroundSuite extends BaseScalaPrinterTest {

  val cover = true
  // val cover = false


  if (cover) {
    val coverageFile = Paths.get("tests/slow/coverage-comments.txt").toFile
    // val regressionPath = Paths.get("tests/slow/regressions-comments.txt").toFile
    val todoFile = coverageFile
    val source = scala.io.Source.fromFile(todoFile)
    val lines = source.getLines
    lines.drop(4)
    val file = lines.next
    
    source.close()
    val base = Paths.get("/home/gui/scala-syntax/tests/slow/target/repos")
    val input = Input.File(AbsolutePath(base.resolve(file)))
    checkComments("playground slow", input, true)  
  } else {
    checkComments("playground resource", resource("playground.scala"))
    // checkSource(resource("playground.scala").text)
  }
}
