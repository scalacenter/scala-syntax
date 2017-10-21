package org.scalafmt.tests

object LiteralSuite extends BaseScalaPrinterTest {
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
