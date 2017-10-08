package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.internal.ScalametaInternal
import scala.meta.testkit.AnyDiff
import scala.meta.testkit.Corpus
import scala.meta.testkit.CorpusFile
import scala.meta.testkit.SyntaxAnalysis
import scala.util.Try
import scala.util.control.NonFatal
import scalafix.diff.DiffUtils
import org.scalafmt.Format
import org.scalafmt.InternalOptions
import org.scalameta.logger
import org.scalatest.Ignore

// Comment out to run these tests, currently it fails with output
// https://gist.github.com/olafurpg/ea44f3567d4117e53ca818b1911f9be9
@Ignore
class BijectionPropertyTest extends BaseScalaPrinterTest {
  test("AST is unchanged") {
    val corpus = Corpus
      .files(Corpus.fastparse)
//      .find(_.filename.contains("actor/ConsistencySpec"))
      .take(1000)
      .toBuffer
      .par
    val diffs = SyntaxAnalysis.run[AnyDiff](corpus) { file =>
      try {
        val in = file.read
        import scala.meta._
        val tree = in.parse[Source].get
        val formatted = Format.format(in)
        val tree2 = formatted.parse[Source].get
        AnyDiff(tree, tree2) :: Nil
      } catch {
        case NonFatal(e) =>
          Nil
      }
    }
    def diff(f: CorpusFile, d: AnyDiff): String = {
      def unified(a: String, b: String) = DiffUtils.unifiedDiff(
        f.filename,
        f.filename + "-formatted",
        a.lines.toList,
        b.lines.toList,
        3
      )
      d match {
        case AnyDiff(a: Tree, b: Tree) =>
          val x = ScalametaInternal.resetOrigin(a).syntax
          val y = ScalametaInternal.resetOrigin(b).syntax
          val result = unified(x, y)
          if (result.nonEmpty) result
          else {
            val res2 = unified(
              Format.format(a.structure, defaultOptions),
              Format.format(b.structure, defaultOptions)
            )
            if (res2.nonEmpty) res2
            else ""
          }
        case _ => d.detailed
      }
    }
    val nonEmptyDiff = diffs.filter { d =>
      Try(diff(d._1, d._2)).fold(t => {
        t.printStackTrace()
        false
      }, { out =>
        if (out.nonEmpty) {
          logger.elem(out)
          true
        } else false
      })
    }
    if (nonEmptyDiff.nonEmpty) fail("diffs.nonEmpty!")
  }

}
