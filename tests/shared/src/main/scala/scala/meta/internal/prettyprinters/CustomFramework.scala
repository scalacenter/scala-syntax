package scala.meta.internal.prettyprinters

import utest.framework._
import utest.runner._
import utest.ufansi

class CustomFramework extends Framework {
  override def formatSingle(
      path: Seq[String],
      r: Result
  ): Option[ufansi.Str] = {
    super.formatSingle(Seq(path.last), r)
  }
}
