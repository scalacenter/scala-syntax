package org.scalafmt.internal
package tokens

import scala.meta._
import scala.meta.tokens.Token._

object SyntaxTokensType {
  import Type._
  import SyntaxTokensUtils._

  implicit class XtensionTypeByNameSyntax(private val tree: ByName)
      extends AnyVal {
    def tokensRightArrow: RightArrow = tree.find[RightArrow].get
  }

  implicit class XtensionTypeImplicitFunctionSyntax(val tree: ImplicitFunction)
      extends AnyVal {
    def tokensImplicit: KwImplicit = tree.find[KwImplicit].get
    def tokensRightArrow: RightArrow =
      tree.findBetween[RightArrow](_.params.last, _.res).get
  }

  implicit class XtensionTypeSelectSyntax(private val tree: Select)
      extends AnyVal {
    def tokensDot: Dot = tree.findBetween[Dot](_.qual, _.name).get
  }

  implicit class XtensionTypeWithSyntax(private val tree: With) extends AnyVal {
    def tokensWith: KwWith = tree.findBetween[KwWith](_.lhs, _.rhs).get
  }

}
