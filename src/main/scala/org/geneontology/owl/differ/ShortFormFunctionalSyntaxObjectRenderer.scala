package org.geneontology.owl.differ

import java.io.{StringWriter, Writer}

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.functional.renderer.FunctionalSyntaxObjectRenderer
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.ShortFormProvider

class ShortFormFunctionalSyntaxObjectRenderer(shortFormProvider: ShortFormProvider) {

  private val factory = OWLManager.getOWLDataFactory

  private object WriterDelegate extends Writer {

    private var delegate = new StringWriter()

    def reset(): Unit = delegate = new StringWriter()

    override def toString: String = delegate.getBuffer.toString

    override def close(): Unit = delegate.close()

    override def flush(): Unit = delegate.flush()

    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = delegate.write(cbuf, off, len)

  }

  private val renderer = new ShortFormFunctionalRenderer(WriterDelegate)

  def render(obj: OWLObject): String = synchronized {
    WriterDelegate.reset()
    obj.accept(renderer)
    return WriterDelegate.toString
  }

  private class ShortFormFunctionalRenderer(writer: Writer) extends FunctionalSyntaxObjectRenderer(ShortFormFunctionalSyntaxObjectRenderer.emptyOntology, writer) {

    override def visit(cls: OWLClass): Unit = write(cls)

    override def visit(cls: OWLObjectProperty): Unit = write(cls)

    override def visit(cls: OWLDataProperty): Unit = write(cls)

    override def visit(cls: OWLAnnotationProperty): Unit = write(cls)

    override def visit(cls: OWLNamedIndividual): Unit = write(cls)

    override def visit(cls: OWLDatatype): Unit = write(cls)

    override def visit(iri: IRI): Unit = visit(factory.getOWLClass(iri))

    private def write(entity: OWLEntity): Unit = writer.write(shortFormProvider.getShortForm(entity))

  }


}

object ShortFormFunctionalSyntaxObjectRenderer {

  private val emptyOntology = OWLManager.createOWLOntologyManager().createOntology()

}