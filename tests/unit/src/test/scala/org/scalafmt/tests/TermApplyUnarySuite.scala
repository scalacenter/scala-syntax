package org.scalafmt.tests

object TermApplyUnarySuite extends BaseScalaPrinterTest {
  check("(!a).b")
  check("!a.b")
  check("!(if (a) b else c)")
  check("!(a op b)")
  check("-(1.s)")
  checkStructural("-(a.s)")
  check("-(1L.s)")
  check("-(1.0D.s)")
  check("-(1.0F.s)")
  check("-(-a)")
  check("-(10.s.s)")
}
