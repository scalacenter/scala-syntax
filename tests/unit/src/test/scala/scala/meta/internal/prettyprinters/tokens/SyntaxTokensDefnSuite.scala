package scala.meta.internal.prettyprinters
package tokens

import SyntaxTokensDefn._

import scala.meta._

object SyntaxTokensDefnSuite extends SyntaxTokensSuiteUtils {
  checkOne[Defn.Class](_.tokensClass)("→class← A")
  checkOne[Defn.Class](_.tokensClass)(
    "@Foo({class Bar; new Bar}) →class← Banana()"
  )
  checkNone[Defn.Class](_.tokensLeftBracket)("class A")
  checkSome[Defn.Class](_.tokensLeftBracket)("class A→[←T]")
  checkNone[Defn.Class](_.tokensRightBracket)("class A")
  checkSome[Defn.Class](_.tokensRightBracket)("class A[T→]←")
  checkSome[Defn.Class](_.tokensRightBracket)("class A[T→]←(a: T)")
  checkNil[Defn.Class](_.tokensCommaTparams)("class A")
  checkNil[Defn.Class](_.tokensCommaTparams)("class A[T]")
  checkAll[Defn.Class](_.tokensCommaTparams)("class A[T1→,←T2→,←T3]")

  checkNil[Defn.Class](commasCtor)("class A")
  checkNil[Defn.Class](commasCtor)("class A(a: A)(b: B)")
  checkAll[Defn.Class](commasCtor)(
    "class A(a: A→,← b: B)(c: C→,← d: D→,← e: E)"
  )
  checkNil[Defn.Class](parensParamss)("class A")
  checkAll[Defn.Class](parensParamss)("class A→(←a: A→)←")
  checkAll[Defn.Class](parensParamss)("class A→(←a: A→)←")
  checkAll[Defn.Class](parensParamss)("class A→(←a: A→)←→(←→)←→(←b: B→)←")
  checkAll[Defn.Class](parensParamss)(
    "class A[@unchecked T] @Inject()→(←a: Int→)←→(←→)←→(←→)←→(←implicit c: C→)←"
  )

}
