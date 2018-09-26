package scala.meta.internal

import scala.meta._
import java.nio.charset.StandardCharsets

package object prettyprinters {
  def resource(path: String): Input =
    Input.Stream(
      this.getClass.getClassLoader.getResourceAsStream(path),
      StandardCharsets.UTF_8
    )
}
