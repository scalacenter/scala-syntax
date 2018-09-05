package scala.meta.internal.prettyprinters
package tokens

import scala.meta.internal.prettyprinters.{ScalaToken => S}

import scala.meta._
import scala.meta.tokens.Token._

import scala.meta.internal.paiges.Doc

object SyntaxTokensMisc {

  import SyntaxTokensUtils._

  implicit class XtensionCtorPrimarySyntax(private val tree: Ctor.Primary)
      extends AnyVal {
    def tokensComma: List[List[Comma]] =
      tree.paramss.map(commaSeparated0(tree))
  }

  implicit class XtensionImportSyntax(private val tree: Import) extends AnyVal {
    def `import`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensImport, S.`import`)
    }
    def tokensImport: KwImport =
      tree.find[KwImport].get
  }

  implicit class XtensionPkgSyntax(private val tree: Pkg) extends AnyVal {
    def `package`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensPackage, S.`package`)
    }
    def tokensPackage: KwPackage = tree.findBefore2[KwPackage](_.name)
  }

  implicit class XtensionPkgObjectSyntax(private val tree: Pkg.Object)
      extends AnyVal {
    def `package`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensPackage, S.`package`)
    }

    def tokensPackage: KwPackage = tree.findBefore2[KwPackage](_.name)
  }

}
