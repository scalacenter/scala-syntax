package org.scalafmt.tests

import org.scalafmt.internal.AssociatedTrivias

import scala.meta._

object AssociatedTriviasSuite extends DiffSuite {
  test("associations") {
    val source =
      """|{
         |  /* java
         |   * doc
         |   */
         |  val a = 1
         |  class A   // trailing
         |}""".stripMargin.parse[Stat].get

    val trivias = AssociatedTrivias(source)
    val obtained = trivias.toString

    val expected =
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
         |)""".stripMargin

    assertNoDiff(obtained, expected)
  }
}
