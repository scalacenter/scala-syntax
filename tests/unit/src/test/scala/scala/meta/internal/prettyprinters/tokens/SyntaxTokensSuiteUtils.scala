package scala.meta.internal.prettyprinters
package tokens

import SyntaxTokens._

import scala.meta._
import scala.meta.Token._
import scala.meta.parsers.Parse

abstract class SyntaxTokensSuiteUtils extends FunSuite {
  val dq = '"'
  val tq = s"${dq}${dq}${dq}"

  def classCommas(cls: Defn.Class): List[Comma] = cls.ctor.tokensComma.flatten

  def checkNone[T <: Tree](f: T => Option[Token])(source: String): Unit = {
    val tree = source.parse[Stat].get.asInstanceOf[T]
    test(source) {
      assert(f(tree).isEmpty)
    }
  }

  def checkSome[T <: Tree](f: T => Option[Token])(annotedSource: String): Unit =
    checkAll[T](tree => List(f(tree).get))(annotedSource)

  // def checkOne[T <: Tree](f: T => Token)(annotedSource: String): Unit =
  //   checkAll[T](tree => List(f(tree)))(annotedSource)

  // def checkOneType[T <: Tree](f: T => Token)(annotedSource: String): Unit =
  //   checkAllType[T](tree => List(f(tree)))(annotedSource)

  // def checkAllType[T <: Tree](
  //     f: T => List[Token]
  // )(annotedSource: String): Unit =
  //   checkAll0[T, Type](f)(annotedSource)

  def checkNil[T <: Tree](f: T => List[Token])(source: String): Unit = {
    checkAll[T](f)(source, isNil = true)
  }

  def checkAll[T <: Tree](
      f: T => List[Token]
  )(annotedSource: String, isNil: Boolean = false): Unit =
    checkAll0[T, Stat](f)(annotedSource, isNil)

  private def checkAll0[T <: Tree, S: Parse](
      f: T => List[Token]
  )(annotedSource: String, isNil: Boolean = false): Unit = {
    val startMarker = '→'
    val stopMarker = '←'
    val nl = "\n"
    val source = annotedSource
      .replaceAllLiterally(startMarker.toString, "")
      .replaceAllLiterally(stopMarker.toString, "")

    var markersOffset = 0
    var i = 0
    val markersBuilder = List.newBuilder[(Int, Int)]
    var lastStart: Option[Int] = None
    def error(msg: String, pos: Int): Unit = {
      test(annotedSource) {
        sys.error(
          msg + nl +
            annotedSource + nl +
            (" " * pos) + "^"
        )
      }
    }
    annotedSource.foreach { c =>
      if (c == startMarker) {
        if (lastStart.nonEmpty)
          error(s"Missing closing marker: '$stopMarker'", i)
        lastStart = Some(i - markersOffset)
        markersOffset += 1
      } else if (c == stopMarker) {
        lastStart match {
          case Some(start) => markersBuilder += ((start, i - markersOffset))
          case None => error("Unexpected closing marker", i)
        }
        lastStart = None
        markersOffset += 1
      }
      i += 1
    }

    val markers = markersBuilder.result()
    if (isNil && markers.size != 0) {
      error("checkNil should not have markers", markers.head._1)
    }

    val markedSource = markers.foldLeft(fansi.Str(source)) {
      case (acc, (start, end)) => acc.overlay(fansi.Color.Yellow, start, end)
    }

    def assertPos(obtained: Int, expected: Int): Unit = {
      assert(
        obtained == expected,
        nl + source + nl +
          (" " * expected) + "^ expected" + nl +
          (" " * obtained) + "^ obtained"
      )
    }

    test(markedSource.toString) {
      val tree = source.parse[S].get.asInstanceOf[T]
      val tokens = f(tree)

      val positionMatch = 
        tokens.zip(markers).forall {
          case (t, (s, e)) => {
            t.pos.start == s &&
            t.pos.end == e
          }
        }

      if (!positionMatch) {
        
        val obtained = 
          tokens.foldLeft(fansi.Str(source)) {
            case (acc, token) => acc.overlay(fansi.Color.Red, token.start, token.end)
          }

        println(
          s"""|position mismatch
              |
              |obtained:
              |  $obtained
              |expected:""".stripMargin
        )
        assert(false)
      }
    }
  }
}
