package scala.meta.internal.prettyprinters

object ScaladocSuite extends BaseScalaPrinterTest {
  // align on right
  check(
    """|trait A {
       |  /**
       |    * L
       |    */
       |  def foo: Int
       |}""".stripMargin
  )

  // align on left
  check(
    """|trait B {
       |  /**
       |   * L
       |   */
       |  def foo: Int
       |}""".stripMargin
  )

  check(
    """|trait A {
       |  
       |/* L */f(0)
       |}""".stripMargin
  )

  check("/*L*/ val d = 1")

  check(
    """|/*
       | L
       |*/
       |class E""".stripMargin
  )

  check(
    """|/*
       | * L
       | */
       |class F""".stripMargin
  )

  checkSource(
    """|package g
       |
       |/**
       | * L
       | */
       |class A""".stripMargin
  )

  checkSource(
    """|package g
       |/**
       | * L
       | */
       |class A""".stripMargin
  )

  check(
    """|/*
       | L
       |*/
       |class I""".stripMargin
  )
  checkSource(
    """|/*
       | L
       |
       | L
       |*/
       |class J""".stripMargin
  )
}
