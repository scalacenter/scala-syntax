package org.scalafmt.tests

import scala.meta.dialects
import org.scalafmt.Options

object ImportSuite extends BaseScalaPrinterTest {
  checkSource("package a.b")
  check("import a.b")
  check("import a.b, c.d")
  check("import a._")
  check("import a.{ b, c }")
  check("import a.{ b => c }")
  check("import a.{ b => _ }")
}
