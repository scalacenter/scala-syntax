package scala.meta.internal.prettyprinters

import scala.meta._

import scala.meta.dialects.Sbt
import scala.meta.parsers.Parse

object AssociatedTriviasSuite extends DiffSuite {

  def check(source: String, expected: String): Unit = {
    val in = source.stripMargin
    check(in.parse[Stat].get, expected)
  }

  def checkSource(input: Input, expected: String): Unit = {
    check(input.parse[Source].get, expected)
  }

  def check(tree: Tree, expected: String): Unit = {
    val trivias = AssociatedTrivias(tree)
    val obtained = trivias.toString
    val expected0 = expected.stripMargin

    if (obtained != expected0) {
      println()
      println(tree.syntax)
      println()
    }
    assertNoDiff(obtained, expected0)
  }

  test("ok") {
    check(
      """|trait A {
         |  /**
         |    * L
         |    */
         |  def foo: Int
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    def [33..36) => [/**¶∙∙∙∙*∙L¶∙∙∙∙*/,¶]
         |  Trailing =
         |
         |)""".stripMargin
    )
  }

  test("basic") {
    check(
      """|object O {
         |  /* java
         |   * doc
         |   */
         |  val a = 1
         |  class A   // trailing
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    val [38..41) => [/*∙java¶∙∙∙*∙doc¶∙∙∙*/,¶]
         |  Trailing =
         |    A [56..57) => [//∙trailing,¶]
         |)"""
    )
  }

  test("separated leading") {
    check(
      """|// test
         |
         |class A""",
      """|AssociatedTrivias(
         |  Leading =
         |    class [9..14) => [//∙test,¶,¶]
         |  Trailing =
         |
         |)""",
    )
  }

  test("single line javadocs") {
    check(
      """|// c1
         |// c2
         |class A""",
      """|AssociatedTrivias(
         |  Leading =
         |    class [12..17) => [//∙c1,¶,//∙c2,¶]
         |  Trailing =
         |
         |)""",
    )
  }

  test("trailing EOF") {
    check(
      "class A // trailing",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    A [6..7) => [//∙trailing]
         |)"""
    )
  }
  test("trailing NL EOF") {
    check(
      """|class A // trailing
         |""",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    A [6..7) => [//∙trailing,¶]
         |)"""
    )
  }

  test("leading EOF") {
    check(
      """|class A
         |// L
         |""",
      """|AssociatedTrivias(
         |  Leading =
         |    EOF [13..13) => [//∙L,¶]
         |  Trailing =
         |
         |)"""
    )
  }

  test("leading vs trailing 1") {
    check(
      """|class A {
         |  val a = /*L*/ b /*T*/
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    b [26..27) => [/*L*/]
         |  Trailing =
         |    b [26..27) => [/*T*/,¶]
         |)""".stripMargin
    )
  }

  test("leading vs trailing 2") {
    check(
      """|class A {
         |  val a = // T
         |    1
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    = [18..19) => [//∙T,¶]
         |)""".stripMargin
    )
  }

  test("leading vs trailing 3") {
    check(
      "class A /* T */ extends B",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    A [6..7) => [/*∙T∙*/]
         |)"""
    )
  }

  test("leading vs trailing 4") {
    check(
      "class A { new B(1 /* C */, 2) }",
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    1 [16..17) => [/*∙C∙*/]
         |)"""
    )
  }

  test("leading simple") {
    check(
      """|def f =
         |  // L
         |  {}""",
      """|AssociatedTrivias(
         |  Leading =
         |    { [17..18) => [//∙L,¶]
         |  Trailing =
         |
         |)"""
    )
  }

  test("full") {
    checkSource(
      resource("comments.scala"),
      """|AssociatedTrivias(
         |  Leading =
         |    { [202..203) => [//∙L∙outer∙block,¶]
         |    } [248..249) => [//∙L∙inner∙block,¶]
         |    ) [338..339) => [//∙L∙inner∙apply,¶]
         |    } [542..543) => [//∙L∙inner∙class,¶]
         |    } [585..586) => [//∙L∙inner∙class∙stats,¶]
         |  Trailing =
         |    , [21..22) => [//∙T∙comma∙apply,¶]
         |    , [72..73) => [//∙T∙comma∙params,¶]
         |    , [122..123) => [//∙T∙comma∙params∙2,¶]
         |    { [202..203) => [//∙T∙inner∙block,¶]
         |    } [248..249) => [//∙T∙outer∙block,¶]
         |    => [291..293) => [//∙T∙match,¶]
         |    { [347..348) => [//∙T∙inner∙new,¶]
         |    { [376..377) => [//∙T∙inner∙apply,¶]
         |    , [422..423) => [//∙T∙comma∙class∙param,¶]
         |    , [471..472) => [//∙T∙comma∙class∙param∙2,¶]
         |)"""
    )
  }
}
