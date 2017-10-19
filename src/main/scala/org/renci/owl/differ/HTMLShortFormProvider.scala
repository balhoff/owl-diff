package org.renci.owl.differ

import org.apache.commons.text.StringEscapeUtils
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.util.ShortFormProvider

class HTMLShortFormProvider(labelProvider: ShortFormProvider) extends ShortFormProvider {

  def getShortForm(entity: OWLEntity): String = StringEscapeUtils.escapeHtml4(labelProvider.getShortForm(entity))

}

class HTMLLinkShortFormProvider(labelProvider: HTMLShortFormProvider) extends ShortFormProvider {

  def getShortForm(entity: OWLEntity): String = {
    val label = labelProvider.getShortForm(entity)
    s"""<a href="${entity.getIRI}">$label</a>"""
  }

}