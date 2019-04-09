package org.geneontology.owl.differ

import com.google.common.base.Optional

object Util {

  implicit class StringOps(val self: String) extends AnyVal {

    def replaceLast(target: Char, replacement: String): String = {
      val lastIndex = self.lastIndexOf(target)
      if (lastIndex > -1) {
        val prefix = self.substring(0, lastIndex)
        val suffix = if (self.length > lastIndex + 1) self.substring(lastIndex + 1)
        else ""
        s"$prefix$replacement$suffix"
      } else self
    }

  }

  implicit class OptionalOption[T](val self: Optional[T]) extends AnyVal {

    def toOption: Option[T] = if (self.isPresent) Option(self.get) else None

  }

  def replaceNewlines(text: String): String = text.replaceAll("\\n", "\\\\n")

}
