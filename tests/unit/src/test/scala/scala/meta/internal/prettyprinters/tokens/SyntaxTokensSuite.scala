package scala.meta.internal.prettyprinters
package tokens

import scala.meta._

import SyntaxTokens._

object SyntaxTokensSuite extends SyntaxTokensSuiteUtils {
  checkNil[Defn.Class](_.ctor.tokensComma.flatten)("class A")
  checkNil[Defn.Class](_.ctor.tokensComma.flatten)("class A(a: A)")
  checkAll[Defn.Class](_.ctor.tokensComma.flatten)("class A(a1: A→,← a2: A)")
  checkAll[Defn.Class](_.ctor.tokensComma.flatten)(
    "class A(a1: A→,← a2: A→,← a3: A)(b1: B→,← b2: B→,← b3: B)"
  )
}
