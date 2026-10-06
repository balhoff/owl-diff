package org.geneontology.owl.differ

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

  def replaceNewlines(text: String): String = text.replaceAll("\\n", "\\\\n")

}
