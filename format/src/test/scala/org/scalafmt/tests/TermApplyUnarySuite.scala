package org.scalafmt.tests

class TermApplyUnarySuite extends BaseScalaPrinterTest {
  check("(!a).b")
  check("!a.b")
  check("!(if (a) b else c)")
  check("!(a op b)")
}
