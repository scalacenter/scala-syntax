package org.scalafmt.internal

import scala.annotation.switch
import scala.meta.Importer
import scala.meta.Init
import scala.meta.Lit
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.internal.fmt.SyntacticGroup
import scala.meta.internal.fmt.{SyntacticGroup => g}
import scala.meta.internal.format.CustomFormatTrees.PatName
import scala.meta.internal.prettyprinters.DoubleQuotes
import scala.meta.internal.prettyprinters.QuoteStyle
import scala.meta.internal.prettyprinters.SingleQuotes
import scala.meta.internal.prettyprinters.TripleQuotes
import org.scalameta.logger

object TreeOps {

  // This method is pessimistic, it assumes all trees requires parens except
  // a small set of whitelisted tree nodes.
  @deprecated
  def needsParens(tree: Tree): Boolean = tree match {
    // format: off
    case
         _: Lit |
         _: Name.Anonymous |
         _: Pat.Extract |
         _: Pat.SeqWildcard |
         _: Pat.Tuple |
         _: Pat.Var |
         _: Pat.Wildcard |
         _: PatName |
         _: Term.Apply |
         _: Term.ApplyType |
         _: Term.Interpolate |
         _: Term.Name |
         _: Term.Placeholder |
         _: Term.Select |
         _: Term.Super |
         _: Term.This |
         _: Type.Apply |
         _: Type.Name |
         _: Type.Select
       => false
    // format: on
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

  def opNeedsParens(
      oo: String,
      io: String,
      customAssoc: Boolean,
      customPrecedence: Boolean,
      side: Side
  ): Boolean = {
    def isleftassoc(name: String): Boolean =
      if (customAssoc) name.last != ':' else true
    def precedence(name: String): Int =
      if (customPrecedence) Term.Name(name).precedence else 0
    val (ol, il) = (isleftassoc(oo), isleftassoc(io))
    if (ol ^ il) true
    else {
      val (l, r) = (ol, !ol)
      val (op, ip) = (precedence(oo), precedence(io))
      if (op < ip) r
      else if (op > ip) l
      else l ^ side.isLeft
    }
  }

  def groupNeedsParens(
      og: SyntacticGroup,
      ig: SyntacticGroup,
      side: Side
  ): Boolean = (og, ig) match {
    case (g.Term.InfixExpr(oo), g.Term.InfixExpr(io)) =>
      opNeedsParens(
        oo,
        io,
        customAssoc = true,
        customPrecedence = true,
        side
      )
    case (g.Type.InfixTyp(oo), g.Type.InfixTyp(io)) =>
      opNeedsParens(
        oo,
        io,
        customAssoc = true,
        customPrecedence = false,
        side
      )
    case (g.Pat.Pattern3(oo), g.Pat.Pattern3(io)) =>
      opNeedsParens(
        oo,
        io,
        customAssoc = true,
        customPrecedence = true,
        side
      )
    case _ => og.precedence > ig.precedence
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
