package org.geneontology.owl.differ

import java.io.{StringWriter, Writer}

import org.apache.commons.text.StringEscapeUtils
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLObjectRenderer
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer
import org.semanticweb.owlapi.model.{IRI, OWLDataVisitor, OWLLiteral, OWLObject}
import org.semanticweb.owlapi.util.{ShortFormProvider, SimpleShortFormProvider}
import org.semanticweb.owlapi.vocab.XSDVocabulary

/**
  * This is a replacement for the OWL API version, in order to force it to render short forms for bare IRIs.
  */
class ManchesterSyntaxOWLObjectRenderer extends OWLObjectRenderer {

  import ManchesterSyntaxOWLObjectRenderer._

  private object WriterDelegate extends Writer {

    private var delegate = new StringWriter()

    def reset(): Unit = delegate = new StringWriter()

    override def toString: String = delegate.getBuffer.toString

    override def close(): Unit = delegate.close()

    override def flush(): Unit = delegate.flush()

    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = delegate.write(cbuf, off, len)

  }

  private var renderer: ManchesterOWLSyntaxObjectRenderer = new BetterIRIRenderer(WriterDelegate, new SimpleShortFormProvider())

  override def render(obj: OWLObject): String = synchronized {
    WriterDelegate.reset()
    obj.accept(renderer)
    return WriterDelegate.toString
  }

  override def setShortFormProvider(shortFormProvider: ShortFormProvider): Unit = synchronized {
    renderer = new BetterIRIRenderer(WriterDelegate, shortFormProvider)
  }

}

object ManchesterSyntaxOWLObjectRenderer {

  private val factory = OWLManager.getOWLDataFactory

  class BetterIRIRenderer(writer: Writer, entityShortFormProvider: ShortFormProvider) extends ManchesterOWLSyntaxObjectRenderer(writer, entityShortFormProvider) {

    override def visit(iri: IRI): Unit = visit(factory.getOWLClass(iri))

    override def visit(node: OWLLiteral): Unit = {

      // xsd:decimal is the default datatype for literal forms like "33.3"
      // with no specified datatype
      if (XSDVocabulary.DECIMAL.getIRI.equals(node.getDatatype.getIRI)) {
        write(node.getLiteral)
      } else if (node.getDatatype.isFloat) {
        write(node.getLiteral)
        write("f")
      } else if (node.getDatatype.isInteger) {
        write(node.getLiteral)
      } else if (node.getDatatype.isBoolean) {
        write(node.getLiteral)
      } else {
        pushTab(getIndent)
        // Change OWL API behavior for next 3 lines
        write("\"")
        write(StringEscapeUtils.escapeHtml4(node.getLiteral))
        write("\"")
        if (node.hasLang()) {
          write("@")
          write(node.getLang)
        } else if (!node.isRDFPlainLiteral) {
          // Change OWL API behavior - indicate xsd:string
          write("^^")
          node.getDatatype.accept(this: OWLDataVisitor)
        }
        popTab()
      }
    }

  }

}
