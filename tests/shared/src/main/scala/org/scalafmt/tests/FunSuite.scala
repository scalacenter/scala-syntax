package org.scalafmt.tests

import utest._, framework._

trait FunSuite extends TestSuite {
  private var N = 0
  private val myTests = IndexedSeq.newBuilder[(String, () => Unit)]
  def testAnon(fun: => Any): Unit = {
    N += 1
    myTests += (N.toString -> (() => fun))
  }
  def test(name: String)(fun: => Any): Unit = {
    myTests += (name -> (() => fun))
  }
  final override def tests: Tests = {
    val ts = myTests.result()
    val names = Tree("", ts.map(x => Tree(x._1)): _*)
    val thunks = new TestCallTree(
      Right(ts.map(x => new TestCallTree(Left(x._2()))))
    )
    Tests.apply(
      names,
      thunks
    )
  }
}
