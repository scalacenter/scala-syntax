package scala.meta.internal.prettyprinters

sealed abstract class Side {
  def isLeft: Boolean = this == Side.Left
}

object Side {
  case object Right extends Side
  case object Left extends Side
}
