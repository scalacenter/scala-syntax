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
}

class B(
    a: Int, // T a
    b: Int
)(
    c: Int, // T c
    d: Int
)
