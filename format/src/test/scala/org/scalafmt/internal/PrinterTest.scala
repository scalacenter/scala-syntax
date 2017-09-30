package org.scalafmt.tests

import scala.meta.testkit.DiffAssertions
import org.scalafmt.Format
import org.scalafmt.InternalOptions
import org.scalameta.logger
import org.scalatest.FunSuite

class PrinterTest extends FunSuite with DiffAssertions {
  def check(original: String, expected: String): Unit = {
    test(logger.revealWhitespace(original)) {
      val obtained = Format.format(original, InternalOptions(30))
      assertNoDiff(obtained, expected)
    }
  }
  check(
    """object foo extends Bar with Fuz(1) with Foooooooooooooooooo {
      |  baaaaaaaaaaaaaaaaaaaaaaaaa(222222222) + 11111.bar(1) + 10 + 10 + 100 + ma.na(1)
      |  }""".stripMargin,
    """
    |
    """.stripMargin
  )
}
