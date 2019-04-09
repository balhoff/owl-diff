package org.geneontology.owl.differ.shortform

import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.util.ShortFormProvider

class MarkdownLinkShortFormProvider(labelProvider: ShortFormProvider) extends ShortFormProvider {

  override def getShortForm(entity: OWLEntity): String = {
    val label = labelProvider.getShortForm(entity)
    val iri = entity.getIRI.toString
    s"[$label]($iri)"
  }

  override def dispose(): Unit = ()

}
