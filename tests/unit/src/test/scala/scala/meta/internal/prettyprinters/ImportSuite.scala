package scala.meta.internal.prettyprinters

object ImportSuite extends BaseScalaPrinterTest {
  checkSource("package a.b")
  check("import a.b")
  check("import a.b, c.d")
  check("import a._")
  check("import a.{ b, c }")
  check("import a.{ b => c }")
  check("import a.{ b => _ }")
}
