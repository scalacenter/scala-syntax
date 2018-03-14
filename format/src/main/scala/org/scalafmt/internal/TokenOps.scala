package org.scalafmt.internal

import scala.meta.Token

object TokenOps {
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

  implicit class XtensionToken(private val token: Token) extends AnyVal {
    def show: String = {
      val name =
        token.getClass.toString.stripPrefix("class scala.meta.tokens.Token$")
      val start = token.pos.start
      val end = token.pos.end
      s"$name [${start}..${end})"
    }
  }
}
