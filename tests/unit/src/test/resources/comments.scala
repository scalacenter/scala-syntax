object A {
  f(
    a, // T comma apply
    b
  )

  def f(
      a: Int, // T comma params
      b: Int
  )(
      c: Int, // T comma params 2
      d: Int
  ) = 1

  def f = 
    // L outer block
    { // T inner block
      // L inner block
    } // T outer block

  m match {
    case a => // T match
  }

  f(
    // L inner apply
  )

  new { // T inner new

  }

  f() { // T inner apply

  }
}

class B(
    a: Int, // T comma class param
    b: Int
)(
    c: Int, // T comma class param 2
    d: Int
)

object A {
  // L inner class
}

object A {
  1
  // L inner class stats
}

object A {
  m match {
    case a: T[_] /* T match strong binding */ =>
  }

  override /* C */ def f = 1


  def /* C f */ f = 1

  a. // T select chain
    b

  if (p) t
  // else T
  else f

  m match { case _ => /* match arrow */ }
}
