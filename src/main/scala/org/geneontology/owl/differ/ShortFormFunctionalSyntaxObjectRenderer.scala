package org.geneontology.owl.differ

import java.io.{StringWriter, Writer}

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.functional.renderer.FunctionalSyntaxObjectRenderer
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.ShortFormProvider
import org.semanticweb.owlapi.vocab.OWLXMLVocabulary
import org.semanticweb.owlapi.vocab.OWLXMLVocabulary.{ANNOTATION_PROPERTY, CLASS, DATATYPE, DATA_PROPERTY, NAMED_INDIVIDUAL, OBJECT_PROPERTY}

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

    private[this] var writeEntityType: Boolean = false

    override def visit(e: OWLClass): Unit = {
      if (writeEntityType) {
        write(CLASS)
        writeOpenBracket()
      }
      write(e)
      if (writeEntityType) {
        writeCloseBracket()
      }
    }

    override def visit(e: OWLObjectProperty): Unit = {
      if (writeEntityType) {
        write(OBJECT_PROPERTY)
        writeOpenBracket()
      }
      write(e)
      if (writeEntityType) {
        writeCloseBracket()
      }
    }

    override def visit(e: OWLDataProperty): Unit = {
      if (writeEntityType) {
        write(DATA_PROPERTY)
        writeOpenBracket()
      }
      write(e)
      if (writeEntityType) {
        writeCloseBracket()
      }
    }

    override def visit(e: OWLAnnotationProperty): Unit = {
      if (writeEntityType) {
        write(ANNOTATION_PROPERTY)
        writeOpenBracket()
      }
      write(e)
      if (writeEntityType) {
        writeCloseBracket()
      }
    }

    override def visit(e: OWLNamedIndividual): Unit = {
      if (writeEntityType) {
        write(NAMED_INDIVIDUAL)
        writeOpenBracket()
      }
      write(e)
      if (writeEntityType) {
        writeCloseBracket()
      }
    }

    override def visit(e: OWLDatatype): Unit = {
      if (writeEntityType) {
        write(DATATYPE)
        writeOpenBracket()
      }
      write(e)
      if (writeEntityType) {
        writeCloseBracket()
      }
    }

    override def visit(iri: IRI): Unit = visit(factory.getOWLClass(iri))

    override def visit(axiom: OWLDeclarationAxiom): Unit = {
      writeEntityType = true
      super.visit(axiom)
      writeEntityType = false
    }

    private def write(entity: OWLEntity): Unit = writer.write(shortFormProvider.getShortForm(entity))

    private def write(v: OWLXMLVocabulary): Unit = writer.write(v.getShortForm)

  }


}

object ShortFormFunctionalSyntaxObjectRenderer {

  private val emptyOntology = OWLManager.createOWLOntologyManager().createOntology()

}