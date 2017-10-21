package org.scalafmt.tests

object TermInterpolateSuite extends BaseScalaPrinterTest {
  check(""" s"$$a" """.stripMargin)
  check(""" s"${_1}" """)
  check(""" s"${`a..n`}" """)
  check("new A(s\"\"\" \"${a.b}\".\"\"\")")
  check(""" q"b" """)
  check(""" q"b\n" """)
  check(" q\"\"\"b\n\"\"\" ")
  check(" q\"b$b\" ")
  check(" q\"b$b$c\" ")
  check(" q\"b$b_$c\" ")
  check(" q\"b${b.c}\" ")
  check(" q\"b${1 + q\"a\"}\" ")
  check(" q\"b${b}_\" ")
  check(
    """
      |foo(s'''
      |''')
      |""".stripMargin,
    """
      |foo(
      |  s'''
      |'''
      |)""".stripMargin
  )

  check(
    """
      |foo(s'''
      | $s
      |''')
      |""".stripMargin,
    """
      |foo(
      |  s'''
      | $s
      |'''
      |)""".stripMargin
  )

}
