package org.renci.owl.differ

import org.apache.commons.text.StringEscapeUtils
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.util.ShortFormProvider

class HTMLShortFormProvider(labelProvider: ShortFormProvider) extends ShortFormProvider {

  def dispose(): Unit = ()

  def getShortForm(entity: OWLEntity): String = {
    val escapedLabel = StringEscapeUtils.escapeHtml4(labelProvider.getShortForm(entity))
    s"""<a href="${entity.getIRI}">$escapedLabel</a>"""
  }

}