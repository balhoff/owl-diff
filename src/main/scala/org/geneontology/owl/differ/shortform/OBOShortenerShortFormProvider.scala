package org.geneontology.owl.differ.shortform

import org.geneontology.owl.differ.Util.StringOps
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.util.ShortFormProvider

class OBOShortenerShortFormProvider(baseProvider: ShortFormProvider) extends ShortFormProvider {

  private val OBOBASE = "http://purl.obolibrary.org/obo/"

  def getShortForm(entity: OWLEntity): String = {
    val base = baseProvider.getShortForm(entity)
    if (base.startsWith("obo:")) base.substring(4).replaceLast('_', ":")
    else if (base.startsWith(OBOBASE)) base.substring(OBOBASE.length).replaceLast('_', ":")
    else base
  }

  override def dispose(): Unit = ()

}
