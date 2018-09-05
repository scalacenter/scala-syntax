package scala.meta.internal.prettyprinters

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
