package org.scalafmt.tests

import scala.meta._

import org.scalafmt.internal.SyntaxTokens._

object SyntaxTokensTypesSuite extends SyntaxTokensSuiteUtils {
  checkOneType[Type.ByName](_.tokensRightArrow)("→=>← A")
  checkOneType[Type.Select](_.tokensDot)("A→.←B")
  // checkOneType[Type.ImplicitFunction](_.tokensImplicit)("→implicit← A => B")
  // checkOneType[Type.ImplicitFunction](_.tokensRightArrow)("implicit A →=>← B")
  // checkOneType[Type.And](_.tokensAnd)("A →&← B")
  // checkOneType[Type.Or](_.tokensOr)("A →|← B") dotty
  checkOneType[Type.With](_.tokensWith)("A →with← B")


  // →←
}