package org.scalafmt.tests

object TermMatchSuite extends BaseScalaPrinterTest {
  check("A((a match { case 1 => }):_*)")
  check("(a => b) match { case _ => }")
  check("(a, b) match { case _ => }")
  check("(a: @b) match { case _ => }")
  check("(if (a) b else c) match { case _ => }")
  check("a -> b match { case _ => }")
  check("a match { case 1 => }")
  check("(a: @switch) match { case _ => b }")
  check("(if (a) b else c) match { case 1 => }")
  check("a match {case _ => { f op { b } } }")
}