package org.scalafmt.tests

import scala.meta._
import scala.meta.parsers.Parse

import org.scalafmt.internal.SyntaxTokens._

trait SyntaxTokensSuiteUtils extends FunSuite {
  val dq = '"'
  val tq = s"${dq}${dq}${dq}"

  def superDot(sel: Term.Select): Option[Token] = {
    val Term.Select(sup: Term.Super, _) = sel
    sup.tokensDot
  }

  /* It's not always possible to write the syntax of a tree node directly
   * for example, Term.Repeated are wraped in Term.Apply: f(x: _*)
   * We can write an arbitary function to extract a child node.
   * ex: checkSome[Term.Param, Decl.Def](_.tokenEqual)("def f(x: A →=← 1): B")
   *   R: Term.Param, T: Decl.Def, apply: extract first parameter
   */
  trait Projection[R, T] {
    def apply(root: R): T
  }

  implicit val repeated: Projection[Term.Apply, Term.Repeated] =
    new Projection[Term.Apply, Term.Repeated] {
      def apply(ap: Term.Apply): Term.Repeated = {
        val Term.Apply(_, List(r: Term.Repeated)) = ap
        r
      }
    }

  implicit val defparam: Projection[Decl.Def, Term.Param] =
    new Projection[Decl.Def, Term.Param] {
      def apply(dec: Decl.Def): Term.Param = dec.paramss.head.head
    }

  implicit val funparam: Projection[Term.Function, Term.Param] =
    new Projection[Term.Function, Term.Param] {
      def apply(fun: Term.Function): Term.Param = fun.params.head
    }

  def checkNone[T <: Tree, R <: Tree](
      f: T => Option[Token]
  )(annotedSource: String)(implicit projection: Projection[R, T]): Unit =
    checkNone[R](tree => f(projection(tree)))(annotedSource)

  def checkNone[T <: Tree](f: T => Option[Token])(source: String): Unit = {
    val tree = source.parse[Stat].get.asInstanceOf[T]
    test(source) {
      assert(f(tree).isEmpty)
    }
  }

  def checkSome[T <: Tree, R <: Tree](
      f: T => Option[Token]
  )(annotedSource: String)(implicit projection: Projection[R, T]): Unit =
    checkSome[R](tree => f(projection(tree)))(annotedSource)

  def checkSome[T <: Tree](f: T => Option[Token])(annotedSource: String): Unit =
    checkAll[T](tree => List(f(tree).get))(annotedSource)

  def checkOne[T <: Tree, R <: Tree](
      f: T => Token
  )(annotedSource: String)(implicit projection: Projection[R, T]): Unit =
    checkOne[R](tree => f(projection(tree)))(annotedSource)

  def checkOne[T <: Tree](f: T => Token)(annotedSource: String): Unit =
    checkAll[T](tree => List(f(tree)))(annotedSource)

  def checkOneType[T <: Tree](f: T => Token)(annotedSource: String): Unit =
    checkAllType[T](tree => List(f(tree)))(annotedSource)

  def checkNil[T <: Tree](f: T => List[Token])(source: String): Unit = {
    val tree = source.parse[Stat].get.asInstanceOf[T]
    test(source) {
      assert(f(tree).isEmpty)
    }
  }

  def checkAllType[T <: Tree](
      f: T => List[Token]
  )(annotedSource: String): Unit =
    checkAll0[T, Type](f)(annotedSource)

  def checkAll[T <: Tree](f: T => List[Token])(annotedSource: String): Unit =
    checkAll0[T, Stat](f)(annotedSource)

  private def checkAll0[T <: Tree, S: Parse](
      f: T => List[Token]
  )(annotedSource: String): Unit = {
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
      sys.error(
        msg + nl +
          annotedSource + nl +
          (" " * i) + "^"
      )
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

    def assertPos(obtained: Int, expected: Int): Unit = {
      assert(
        obtained == expected,
        nl + source + nl +
          (" " * expected) + "^ expected" + nl +
          (" " * obtained) + "^ obtained"
      )
    }

    test(annotedSource) {
      val tree = source.parse[S].get.asInstanceOf[T]
      val tokens = f(tree)
      tokens.zip(markers).foreach {
        case (t, (s, e)) => {
          assertPos(t.pos.start, s)
          assertPos(t.pos.end, e)
        }
      }
      assert(
        tokens.size == markers.size,
        s"incorrect number of tokens, expected: ${markers.size}, obtained: ${tokens.size}"
      )
    }
  }
}
