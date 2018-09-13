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
