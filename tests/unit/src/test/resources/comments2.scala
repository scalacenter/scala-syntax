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
