package org.scalafmt.internal

import scala.meta.Lit
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.internal.format.FormatTree.PatName
import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.SingleQuotes
import scala.meta.internal.prettyprinters.TripleQuotes

object TreeOps {

  // This method is pessimistic, it assumes all trees requires parens except
  // a small set of whitelisted tree nodes.
  def needsParens(tree: Tree): Boolean = tree match {
    case _: Term.Name | _: Type.Name | _: Name.Anonymous | _: Lit |
        _: Term.Interpolate | _: Term.Apply | _: Term.ApplyType |
        _: Type.Apply | _: Term.Select | _: Type.Select | _: Term.Super |
        _: Term.This | _: Pat.Var | _: Pat.Tuple | _: PatName | _: Pat.Extract |
        _: Term.Placeholder | _: Pat.Wildcard | _: Pat.SeqWildcard =>
      false
    case t: Term.New => t.init.argss.isEmpty
//    case t: Term.Annotate => false // NOTE(olafur) because we always wrap Term.Annotate
    case _ => true
  }

  object Infix {
    def unapply(arg: Tree): Option[String] = arg match {
      case Term.ApplyInfix(_, Term.Name(op), _, _) => Some(op)
      case Type.ApplyInfix(_, Type.Name(op), _) => Some(op)
      case Pat.ExtractInfix(_, Term.Name(op), _) => Some(op)
      case _ => None
    }
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
  import scala.meta.internal.trees._
  // http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#assignment-operators
  def isAssignmentOperator(op: String): Boolean =
    Term.Name(op).isAssignmentOp

  // http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations
  def operatorPrecedence(op: String): Int =
    Term.Name(op).precedence

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
