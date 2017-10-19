package org.renci.owl.differ

import org.apache.commons.text.StringEscapeUtils
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.util.IRIShortFormProvider

class HTMLSafeIRIShortFormProvider(baseProvider: IRIShortFormProvider) extends IRIShortFormProvider {

  def getShortForm(iri: IRI): String = {
    println(s"Get IRI short form: $iri")
    StringEscapeUtils.escapeHtml4(baseProvider.getShortForm(iri))
  }

}