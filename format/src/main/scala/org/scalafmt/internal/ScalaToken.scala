package org.scalafmt.internal

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.char
import org.typelevel.paiges.Doc.lineBreak
import org.typelevel.paiges.Doc.text

object ScalaToken {
  val `@` : Doc = char('@')
  val `*` : Doc = char('*')
  val `.` : Doc = char('.')
  val `(` : Doc = char('(')
  val `)` : Doc = char(')')
  val `[` : Doc = char('[')
  val `]` : Doc = char(']')
  val `{` : Doc = char('{')
  val `}` : Doc = char('}')
  val `:` : Doc = char(':')
  val `=` : Doc = char('=')
  val wilcard: Doc = char('_')
  val `this` : Doc = text("this")
  val `=>` : Doc = text("=>")
  val `<-` : Doc = text("<-")
  val `case`: Doc = text("case")
  val `class`: Doc = text("class")
  val `def`: Doc = text("def")
  val `else`: Doc = text("else")
  val `extends`: Doc = text("extends")
  val `false`: Doc = text("false")
  val `final`: Doc = text("final")
  val `for`: Doc = text("for")
  val `yield`: Doc = text("yield")
  val `try`: Doc = text("try")
  val `catch`: Doc = text("catch")
  val `finally`: Doc = text("finally")
  val `if`: Doc = text("if")
  val `import`: Doc = text("import")
  val `match`: Doc = text("match")
  val `null`: Doc = text("null")
  val `return`: Doc = text("return")
  val `object`: Doc = text("object")
  val `package`: Doc = text("package")
  val `private`: Doc = text("private")
  val `protected`: Doc = text("protected")
  val `trait`: Doc = text("trait")
  val `true`: Doc = text("true")
  val `val`: Doc = text("val")
  val `var`: Doc = text("var")
  val `with`: Doc = text("with")
  val lineBlank: Doc = lineBreak + lineBreak
}
