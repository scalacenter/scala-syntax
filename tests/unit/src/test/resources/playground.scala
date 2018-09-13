object A {
  f { b => implicit c => d }

  f { implicit b => implicit c => d }

  f(
    b, { implicit c =>
      d

      e
    }
  )
}
