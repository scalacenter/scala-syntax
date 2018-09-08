package scala.meta.internal.prettyprinters

import scala.meta._

object AssociatedTriviasSuite extends DiffSuite {

  def check(source: String, expected: String): Unit = {
    val trivias = AssociatedTrivias(source.stripMargin.parse[Stat].get)
    val obtained = trivias.toString
    assertNoDiff(obtained, expected.stripMargin)
  }

  test("ok") {
    check(
      """|trait A {
         |  /**
         |    * L
         |    */
         |  def foo: Int
         |}""".stripMargin,
      """|AssociatedTrivias(
         |  Leading =
         |    def [33..36) => [∙,∙,/**¶∙∙∙∙*∙L¶∙∙∙∙*/,¶,∙,∙]
         |  Trailing =
         |    trait [0..5) => [∙]
         |    A [6..7) => [∙]
         |    { [8..9) => [¶]
         |    def [33..36) => [∙]
         |    : [40..41) => [∙]
         |    Int [42..45) => [¶]
         |)""".stripMargin
    )
  }

  test("basic") {
    check(
      """|{
         |  /* java
         |   * doc
         |   */
         |  val a = 1
         |  class A   // trailing
         |}""",
      """|AssociatedTrivias(
         |  Leading =
         |    val [29..32) => [∙,∙,/*∙java¶∙∙∙*∙doc¶∙∙∙*/,¶,∙,∙]
         |    class [41..46) => [∙,∙]
         |  Trailing =
         |    { [0..1) => [¶]
         |    val [29..32) => [∙]
         |    a [33..34) => [∙]
         |    = [35..36) => [∙]
         |    1 [37..38) => [¶]
         |    class [41..46) => [∙]
         |    A [47..48) => [∙,∙,∙,//∙trailing,¶]
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
         |    class [9..14) => [∙]
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
         |    class [12..17) => [∙]
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
         |    class [0..5) => [∙]
         |    A [6..7) => [∙,//∙trailing]
         |)"""
    )
  }
  test("trailing NL EOF") {
    check(
      """|class A // trailing
         |""".stripMargin,
      """|AssociatedTrivias(
         |  Leading =
         |
         |  Trailing =
         |    class [0..5) => [∙]
         |    A [6..7) => [∙,//∙trailing,¶]
         |)"""
    )
  }

  test("leading EOF") {
    check(
      """|class A
         |// L
         |""".stripMargin,
      """|AssociatedTrivias(
         |  Leading =
         |    EOF [13..13) => [//∙L,¶]
         |  Trailing =
         |    class [0..5) => [∙]
         |    A [6..7) => [¶]
         |)"""
    )
  }
}
