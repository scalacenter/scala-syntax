package org.scalafmt.tests

import scala.meta._
import scala.meta.internal.format.Comments._
import org.scalafmt.internal.TreePrinter

object DocstringSuite extends BaseScalaPrinterTest {
  test("docstring") {
    val foo = q"def foo = 2".withLeadingComment("/** Returns 2 */\n")
    val bar = q"def bar = 3".withTrailingComment(" // FIXME")
    val qux = q"def qux = 4".withLeadingComment(
      """/**
        |  * Example multiline docstring
        |  *
        |  * @param a The "a"
        |  */
        |""".stripMargin
    )
    val zzz = q"def zzz = 5"
      .withLeadingComment("// Comment 2\n") // Note: reverse order, change?
      .withLeadingComment("// Comment 1\n")
    val Foo =
      q"class Foo { $foo; $bar; $qux; $zzz }"
        .withLeadingComment("/** Foo is great */\n")
    val source = source"""
package a.b
import b.c
$Foo
"""
    val obtained = TreePrinter.print(source).render(100)
    // Note that indentation is corrected :D
    val expected =
      """
        |package a.b
        |import b.c
        |
        |/** Foo is great */
        |class Foo {
        |  /** Returns 2 */
        |  def foo = 2
        |
        |  def bar = 3 // FIXME
        |
        |  /**
        |    * Example multiline docstring
        |    *
        |    * @param a The "a"
        |    */
        |  def qux = 4
        |
        |  // Comment 1
        |  // Comment 2
        |  def zzz = 5
        |}
      """.stripMargin
    assertNoDiff(obtained, expected)
  }
}
