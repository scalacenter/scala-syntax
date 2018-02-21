package org.scalafmt.tests

object PlaygroundSuite extends BaseScalaPrinterTest {
  check("a match {case _ => { f op { b } } }")
}
