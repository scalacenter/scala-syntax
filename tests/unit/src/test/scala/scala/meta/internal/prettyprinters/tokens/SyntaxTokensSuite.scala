package scala.meta.internal.prettyprinters
package tokens

import scala.meta._
import scala.meta.Token._

import SyntaxTokens._

object SyntaxTokensSuite extends SyntaxTokensSuiteUtils {

  checkNil[Defn.Class](classCommas)("class A")
  checkNil[Defn.Class](classCommas)("class A(a: A)")
  checkAll[Defn.Class](classCommas)("class A(a1: A→,← a2: A)")
  checkAll[Defn.Class](classCommas)(
    "class A(a1: A→,← a2: A→,← a3: A)(b1: B→,← b2: B→,← b3: B)"
  )

  checkNone[Defn.Object](_.templ.tokensRightBrace)("object A")
  checkSome[Defn.Object](_.templ.tokensRightBrace)("object A { →}←")
  checkSome[Defn.Object](_.templ.tokensRightBrace)("object A { def f = { } →}←")

  checkNone[Defn.Object](_.templ.tokensLeftBrace)("object A")
  checkSome[Defn.Object](_.templ.tokensLeftBrace)("object A →{← }")
  checkSome[Defn.Object](_.templ.tokensLeftBrace)(
    "object A extends A with C→{← _: self => }"
  )
}
