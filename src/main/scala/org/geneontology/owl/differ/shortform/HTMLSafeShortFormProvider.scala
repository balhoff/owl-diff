package org.geneontology.owl.differ.shortform

import org.apache.commons.text.StringEscapeUtils
import org.semanticweb.owlapi.model.{IRI, OWLEntity}
import org.semanticweb.owlapi.util.{IRIShortFormProvider, ShortFormProvider}

class HTMLSafeShortFormProvider(labelProvider: ShortFormProvider) extends ShortFormProvider {

  def getShortForm(entity: OWLEntity): String = StringEscapeUtils.escapeHtml4(labelProvider.getShortForm(entity))

  override def dispose(): Unit = ()

}

class HTMLLinkShortFormProvider(labelProvider: HTMLSafeShortFormProvider) extends ShortFormProvider {

  def getShortForm(entity: OWLEntity): String = {
    val label = labelProvider.getShortForm(entity)
    s"""<a href="${entity.getIRI}">$label</a>"""
  }

  override def dispose(): Unit = ()

}

class HTMLSafeIRIShortFormProvider(baseProvider: IRIShortFormProvider) extends IRIShortFormProvider {

  def getShortForm(iri: IRI): String = StringEscapeUtils.escapeHtml4(baseProvider.getShortForm(iri))

}