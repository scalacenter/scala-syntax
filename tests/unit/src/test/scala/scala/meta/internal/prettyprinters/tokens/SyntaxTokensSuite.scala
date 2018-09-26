package scala.meta.internal.prettyprinters
package tokens

import scala.meta._
import scala.meta.Token._

import SyntaxTokens._

object SyntaxTokensSuite extends SyntaxTokensSuiteUtils {

  checkNone[Defn.Object](_.templ.tokensRightBrace)("object A")
  checkSome[Defn.Object](_.templ.tokensRightBrace)("object A { →}←")
  checkSome[Defn.Object](_.templ.tokensRightBrace)("object A { def f = { } →}←")

  checkNone[Defn.Object](_.templ.tokensLeftBrace)("object A")
  checkSome[Defn.Object](_.templ.tokensLeftBrace)("object A →{← }")
  checkSome[Defn.Object](_.templ.tokensLeftBrace)(
    "object A extends A with C→{← _: self => }"
  )
}
