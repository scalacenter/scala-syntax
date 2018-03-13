package org.scalafmt.internal

import scala.meta.{Token, Tokens}

object TokenOps {
  implicit class XtensionTokens(val tokens: Tokens) extends AnyVal {
    def binarySearch(token: Token): Option[Int] = {
      def loop(lo: Int, hi: Int): Int = {
        if (lo > hi) -1
        else {
          val mid = lo + ((hi - lo) / 2)
          val guess = tokens(mid)
          if (guess == token) mid
          else if (guess.end < token.end) loop(mid + 1, hi)
          else loop(lo, mid - 1)
        }
      }
      val res = loop(0, tokens.length - 1)
      if (res == -1) None
      else Some(res)
    }
  }

  type TokenHash = Long
  def hash(token: Token): TokenHash = {
    val longHash: Long =
      (token.productPrefix.hashCode.toLong << (62 - 8)) |
        (token.start.toLong << (62 - (8 + 28))) | token.end
    longHash
  }

  /** Returns true if this token is an identifier that requires a leading space before colon.
   *
   * Example:
   *   needsLeadingSpaceBeforeColon(foo_) // true
   *   needsLeadingSpaceBeforeColon(foo)  // false
   *   val foo_ : Int = 2 // OK
   *   val foo_: Int = 2  // ERROR
   *   val foo: Int = 2   // OK
   *
    **/
  def needsLeadingSpaceBeforeColon(name: String): Boolean =
    name match {
      case "_" => false
      case _ =>
        name.lastOption.exists {
          case '`' => false
          case ch => !ch.isLetterOrDigit
        }
    }

  def isIdentifierStart(value: String): Boolean =
    value.nonEmpty && (Character.isLetterOrDigit(value.head) || value.head == '_')
}
