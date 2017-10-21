package org.scalafmt.tests

class LiteralSuite extends BaseScalaPrinterTest {
  check(
    """
      |foo('''
      |''')
      |""".stripMargin,
    """
      |foo(
      |  '''
      |'''
      |)""".stripMargin
  )
  check("'c'")
}
