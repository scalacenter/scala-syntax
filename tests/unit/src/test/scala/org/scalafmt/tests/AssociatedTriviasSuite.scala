package org.scalafmt.tests

import org.scalafmt.internal.AssociatedTrivias

import scala.meta._

object AssociatedTriviasSuite extends DiffSuite {

  def check(source: String, expected: String): Unit = {
    val trivias = AssociatedTrivias(source.stripMargin.parse[Stat].get)
    val obtained = trivias.toString
    assertNoDiff(obtained, expected.stripMargin)
  }

  test("associations") {
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
         |    val [29..32) => [∙]
         |    a [33..34) => [∙]
         |    = [35..36) => [∙]
         |    class [41..46) => [∙]
         |    A [47..48) => [∙,∙,∙,//∙trailing]
         |)"""
    )
  }

  test("associations 2") {
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
}
