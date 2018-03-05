package org.scalafmt.internal

import scala.meta.{Term, Tree, Lit}
import scala.meta.internal.fmt.SyntacticGroup
import scala.meta.internal.fmt.{SyntacticGroup => g}
import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.SingleQuotes
import scala.meta.internal.prettyprinters.TripleQuotes

object TreeOps {
  def operatorNeedsParenthesis(
      outerOperator: String,
      innerOperator: String,
      customAssociativity: Boolean,
      customPrecedence: Boolean,
      side: Side
  ): Boolean = {
    // The associativity of an operator is determined by the operator's last character.
    // Operators ending in a colon ‘:’ are right-associative. All
    // other operators are left-associative.
    // https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#infix-operations
    def isLeftAssociative(name: String): Boolean =
      if (customAssociativity) name.last != ':' else true

    def precedence(name: String): Int =
      if (customPrecedence) Term.Name(name).precedence else 0

    val outerOperatorIsLeftAssociative = isLeftAssociative(outerOperator)
    val innerOperatorIsLeftAssociative = isLeftAssociative(innerOperator)

    if (outerOperatorIsLeftAssociative ^ innerOperatorIsLeftAssociative) true
    else {
      val isLeft = outerOperatorIsLeftAssociative
      val isRight = !outerOperatorIsLeftAssociative

      val outerOperatorPrecedence = precedence(outerOperator)
      val innerOperatorPrecedence = precedence(innerOperator)

      if (outerOperatorPrecedence < innerOperatorPrecedence) isRight
      else if (outerOperatorPrecedence > innerOperatorPrecedence) isLeft
      else isLeft ^ side.isLeft
    }
  }

  def startsWithNumericLiteral(tree: Tree): Boolean = {
    tree match {
      case _: Lit.Int | _: Lit.Long | _: Lit.Double | _: Lit.Float |
          _: Lit.Byte | _: Lit.Short =>
        true
      case Term.Select(tree0, _) => startsWithNumericLiteral(tree0)
      case _ => false
    }
  }

  def groupNeedsParenthesis(
      outerGroup: SyntacticGroup,
      innerGroup: SyntacticGroup,
      side: Side
  ): Boolean = (outerGroup, innerGroup) match {
    case (g.Term.InfixExpr(outerOperator), g.Term.InfixExpr(innerOperator)) =>
      operatorNeedsParenthesis(
        outerOperator,
        innerOperator,
        customAssociativity = true,
        customPrecedence = true,
        side
      )
    case (g.Type.InfixTyp(outerOperator), g.Type.InfixTyp(innerOperator)) =>
      operatorNeedsParenthesis(
        outerOperator,
        innerOperator,
        customAssociativity = true,
        customPrecedence = false,
        side
      )
    case (g.Pat.Pattern3(outerOperator), g.Pat.Pattern3(innerOperator)) =>
      operatorNeedsParenthesis(
        outerOperator,
        innerOperator,
        customAssociativity = true,
        customPrecedence = true,
        side
      )

    case (_: g.Term.PrefixExpr, g.Term.PrefixArg(_, _: g.Term.PrefixExpr)) =>
      true

    case (g.Term.PrefixExpr("-"), g.Term.PrefixArg(Term.Select(tree, _), _))
        if startsWithNumericLiteral(tree) =>
      true
    case _ =>
      outerGroup.precedence > innerGroup.precedence
  }

}

object SyntaxOps {
  def escape(s: String, style: QuoteStyle): String = {
    val sb = new StringBuilder()
    if (style == TripleQuotes) {
      // TODO(olafur) escape triple quotes
      sb.append(s)
    } else {
      s.foreach {
        case '\t' => sb.append("\\t")
        case '\b' => sb.append("\\b")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\f' => sb.append("\\f")
        case '\\' => sb.append("\\\\")
        case '"' if style eq DoubleQuotes =>
          sb.append("\\\"")
        case '\'' if style eq SingleQuotes =>
          sb.append("\\\'")
        case c =>
          sb.append(c)
      }
    }
    sb.toString
  }
}

object TokenOps {

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
