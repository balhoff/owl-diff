package org.geneontology.owl.differ.shortform

import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.util.ShortFormProvider

class DoubleShortFormProvider(mainProvider: ShortFormProvider, accessoryProvider: ShortFormProvider) extends ShortFormProvider {

  def getShortForm(entity: OWLEntity): String = {
    val mainShortForm = mainProvider.getShortForm(entity)
    val accessoryShortForm = accessoryProvider.getShortForm(entity)
    val usableMainShortForm = if (mainShortForm.startsWith("<") && mainShortForm.endsWith(">"))
      mainShortForm
    else s"<$mainShortForm>"
    val usableAccessoryShortForm = if (accessoryShortForm != entity.toString && accessoryShortForm != mainShortForm)
      s"[$accessoryShortForm]"
    else ""
    s"$usableMainShortForm$usableAccessoryShortForm"
  }

  override def dispose(): Unit = ()

}
