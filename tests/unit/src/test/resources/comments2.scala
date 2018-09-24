package object A {
  // L inner package object
}

package object A {
  1
  // L inner package object stats
}

object O {
  f(x => /* Term.Function T */ ())

  a op { _ => /*T*/ }

  ( // Term.Function T (
    a, // Term.Function T ,
    b
    // Term.Function L )
  ) => a

  a match { case _ => { /* Pat Block T { */ } }
}

class X
class X(/*Defn.Class T ( 1 */)
class X(/*Defn.Class T ( 2 */a: A
  /*Defn.Class L ) 1 */
)
class X(/*Defn.Class T ( 3 */a: A, b: B
  /*Defn.Class L ) 2 */
)
class X(/*Defn.Class T ( 4 */a: A)(
  /*Defn.Class L ) 3 */
)

class X ( // Defn.Class T ( 5
  a: A, // T , 1
  b: B
  /*Defn.Class L ) 4 */
)(
  c: C, // T, 2
  d: D
  /*Defn.Class L ) 5 */
)

object A {
  new A ( // Init T ( 1
    a, // Init T , 2
    b
    // Init L ) 1
  )( // Init T ( 2
    // Init L ) 2
  )

  new B

  new C ()

  new Z {

  }

  new Z {
    a()()
  }
}

class A
  // Templ L extends
  extends // Templ T extends
  B
  // Templ L with
  with // Templ T with
  C

class A
  // Templ L extends
  extends // Templ T extends
  B
  // Templ L with 1
  with // Templ T with 1
  C
  // Templ L with 2
  with // Templ T with 2
  D


object A {
  new B
    // Templ L with 3
    with // Templ T with 3
    C
    // Templ L with 4
    with // Templ T with 4
    D
}

class A 
  // Templ L extends early
  extends // Templ T extends early
  { val a = 1 }
  // Templ L with early
  with // Templ T with early
  B
  // Templ L with
  with // Templ T with
  C

object A {
  f { // Term.PartialFunction T {
    case e => ()
    // Term.PartialFunction L }
  }
}

class A() // Ctor.Primary withTrailingRightParen T 
  extends B

class C[ // Defn.Class tparams T [
  T1, // Defn.Class tparams T , 1
  T2, // Defn.Class tparams T , 2
  T3
  // Defn.Class tparams L ]
]

class A extends C[ // Type.Apply T [
  T1, // Type.Apply T , 1
  T2, // Type.Apply T , 2
  T3
  // Type.Apply L ]
]

object A {
  { 
    case _ => // Case T =>
      ()
  }

  val a 
    // Defn.Val L =
    = // Defn.Val T =
    1


  m match { // Term.Match T {
    case _ => 
    // Term.Match L }
  }

  try {f} catch { // Term.Try T {
    case e => ()
    // Term.Try L } catch
  }
}
