package org.geneontology.owl.differ.render

import org.apache.commons.io.output.ByteArrayOutputStream
import org.geneontology.owl.differ.Differ.BasicDiff
import org.geneontology.owl.differ.ShortFormFunctionalSyntaxObjectRenderer
import org.semanticweb.owlapi.model.{OWLImportsDeclaration, OWLObject}
import org.semanticweb.owlapi.util.ShortFormProvider

import java.io.{OutputStream, PrintWriter, Writer}
import java.nio.charset.StandardCharsets
import scala.util.Using

object BasicDiffRenderer {

  def renderPlainStream(diff: BasicDiff, stream: OutputStream): Unit =
    Using.resource(new PrintWriter(stream)) { writer =>
      renderPlainWriter(diff, writer)
    }

  def renderPlainWriter(diff: BasicDiff, givenWriter: Writer): Unit =
    Using.resource(new PrintWriter(givenWriter)) { writer =>
      if (diff.isEmpty) writer.println("Ontologies are identical")
      else {
        val (left, right) = groups(diff)
        val leftRendered = left.map(_.item.toString)
        val rightRendered = right.map(_.item.toString)
        if ((diff.left.id == diff.right.id) || (diff.left.id.isAnonymous && diff.right.id.isAnonymous)) format(leftRendered, rightRendered, writer)
        else format(leftRendered + diff.left.id.toString, rightRendered + diff.right.id.toString, writer)
      }
    }

  def renderPlain(diff: BasicDiff): String =
    Using.resource(new ByteArrayOutputStream()) { stream =>
      renderPlainStream(diff, stream)
      stream.toString(StandardCharsets.UTF_8)
    }

  def renderStream(diff: BasicDiff, shortFormProvider: ShortFormProvider, stream: OutputStream): Unit =
    Using.resource(new PrintWriter(stream)) { writer =>
      renderWriter(diff, shortFormProvider, writer)
    }

  def renderWriter(diff: BasicDiff, shortFormProvider: ShortFormProvider, givenWriter: Writer): Unit =
    Using.resource(new PrintWriter(givenWriter)) { writer =>
      if (diff.isEmpty) writer.println("Ontologies are identical")
      else {
        val renderer = new ShortFormFunctionalSyntaxObjectRenderer(shortFormProvider)
        val (left, right) = groups(diff)
        val leftRendered = left.map {
          case OWLObjectItem(item) => renderer.render(item)
          case OWLImportItem(item) => item.toString
        }
        val rightRendered = right.map {
          case OWLObjectItem(item) => renderer.render(item)
          case OWLImportItem(item) => item.toString
        }
        if ((diff.left.id == diff.right.id) || (diff.left.id.isAnonymous && diff.right.id.isAnonymous)) format(leftRendered, rightRendered, writer)
        else format(leftRendered + diff.left.id.toString, rightRendered + diff.right.id.toString, writer)
      }
    }

  def render(diff: BasicDiff, shortFormProvider: ShortFormProvider): String =
    Using.resource(new ByteArrayOutputStream()) { stream =>
      renderStream(diff, shortFormProvider, stream)
      stream.toString(StandardCharsets.UTF_8)
    }

  private def groups(diff: BasicDiff): (Set[OWLItem[_]], Set[OWLItem[_]]) = {
    val leftUnique: Set[OWLItem[_]] = diff.left.imports.map(OWLImportItem) ++ diff.left.annotations.map(OWLObjectItem) ++ diff.left.axioms.map(OWLObjectItem)
    val rightUnique: Set[OWLItem[_]] = diff.right.imports.map(OWLImportItem) ++ diff.right.annotations.map(OWLObjectItem) ++ diff.right.axioms.map(OWLObjectItem)
    (leftUnique, rightUnique)
  }

  private def format(removedLines: Set[String], addedLines: Set[String], writer: PrintWriter): Unit = {
    import org.geneontology.owl.differ.Util.replaceNewlines
    val removedSorted = removedLines.map(replaceNewlines).map(ax => s"- $ax").toSeq.sorted
    val addedSorted = addedLines.map(replaceNewlines).map(ax => s"+ $ax").toSeq.sorted
    writer.println(s"${removedLines.size} axioms in left ontology but not in right ontology:")
    removedSorted.foreach(writer.println)
    writer.println()
    writer.println(s"${addedLines.size} axioms in right ontology but not in left ontology:")
    addedSorted.foreach(writer.println)
  }

  private sealed trait OWLItem[T] {

    def item: T

  }

  private final case class OWLObjectItem(item: OWLObject) extends OWLItem[OWLObject]

  private final case class OWLImportItem(item: OWLImportsDeclaration) extends OWLItem[OWLImportsDeclaration]

}
