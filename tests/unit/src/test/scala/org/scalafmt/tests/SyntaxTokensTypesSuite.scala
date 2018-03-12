package org.scalafmt.tests

import scala.meta._

import org.scalafmt.internal.SyntaxTokens._

object SyntaxTokensTypesSuite extends SyntaxTokensSuiteUtils {
  checkOneType[Type.ByName](_.tokensRightArrow)("→=>← A")
  checkOneType[Type.Select](_.tokensDot)("A→.←B")
  // checkOneType[Type.ImplicitFunction](_.tokensImplicit)("→implicit← A => B")
  // checkOneType[Type.ImplicitFunction](_.tokensRightArrow)("implicit A →=>← B")
  
  // →←
}