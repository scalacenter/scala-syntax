package org.scalafmt.tests

import scala.meta._

import org.scalafmt.internal.SyntaxTokens._

object SyntaxTokensDefnSuite extends SyntaxTokensSuiteUtils {
  checkOne[Defn.Class](_.tokensClass)("→class← A")
  checkNone[Defn.Class](_.tokensLeftBracket)("class A")
  checkSome[Defn.Class](_.tokensLeftBracket)("class A→[←T]")
  checkNone[Defn.Class](_.tokensRightBracket)("class A")
  checkSome[Defn.Class](_.tokensRightBracket)("class A[T→]←")
  checkSome[Defn.Class](_.tokensRightBracket)("class A[T→]←(a: T)")
  checkNil[Defn.Class](_.tokensCommaTparams)("class A")
  checkNil[Defn.Class](_.tokensCommaTparams)("class A[T]")
  checkAll[Defn.Class](_.tokensCommaTparams)("class A[T1→,←T2→,←T3]")
  checkNil[Defn.Class](_.tokensCommaCtor.flatten)("class A")
  checkNil[Defn.Class](_.tokensCommaCtor.flatten)("class A(a: A)(b: B)")
  checkAll[Defn.Class](_.tokensCommaCtor.flatten)(
    "class A(a: A→,← b: B)(c: C→,← d: D→,← e: E)"
  )
  checkNone[Defn.Class](_.tokensImplicit)("class A")
  checkSome[Defn.Class](_.tokensImplicit)("class A(→implicit← a: A)")
  checkSome[Defn.Class](_.tokensImplicit)("class A()(→implicit← a: A)")
  checkSome[Defn.Class](_.tokensImplicit)(
    "class A(a: A {def f(implicit ev: E)})(→implicit← a: A)"
  )
  // →←
}
