object A {
  f(
    a, // T
    b
  )

  def f(
      a: Int, // T a
      b: Int
  )(
      c: Int, // T c
      d: Int
  ) = 1

  def f = 
    // L1
    { // T1
      // L2
    } // T2

  m match {
    case a => // T
  }

  f(
    // L
  )

  new { // T

  }

  f() { // T f

  }
}

class B(
    a: Int, // T a
    b: Int
)(
    c: Int, // T c
    d: Int
)

object A {
  // T
}

object A {
  1
  // T
}

package object A {
  // T
}

package object A {
  1
  // T
}
