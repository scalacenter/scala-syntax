package scala.meta.internal.prettyprinters

import scala.meta.internal.prettyprinters._

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
