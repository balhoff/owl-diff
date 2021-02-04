package org.geneontology.owl.differ.render

import org.geneontology.owl.differ.Differ.BasicDiff
import org.geneontology.owl.differ.ShortFormFunctionalSyntaxObjectRenderer
import org.semanticweb.owlapi.model.{OWLImportsDeclaration, OWLObject}
import org.semanticweb.owlapi.util.ShortFormProvider

object BasicDiffRenderer {

  def renderPlain(diff: BasicDiff): String = {
    if (diff.isEmpty) "Ontologies are identical"
    else {
      val (left, right) = groups(diff)
      val leftRendered = left.map(_.item.toString)
      val rightRendered = right.map(_.item.toString)
      if ((diff.left.id == diff.right.id) || (diff.left.id.isAnonymous && diff.right.id.isAnonymous)) format(leftRendered, rightRendered)
      else format(leftRendered + diff.left.id.toString, rightRendered + diff.right.id.toString)
    }
  }

  def render(diff: BasicDiff, shortFormProvider: ShortFormProvider): String = {
    if (diff.isEmpty) "Ontologies are identical"
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
      if ((diff.left.id == diff.right.id) || (diff.left.id.isAnonymous && diff.right.id.isAnonymous)) format(leftRendered, rightRendered)
      else format(leftRendered + diff.left.id.toString, rightRendered + diff.right.id.toString)
    }
  }

  private def groups(diff: BasicDiff): (Set[OWLItem[_]], Set[OWLItem[_]]) = {
    val leftUnique: Set[OWLItem[_]] = diff.left.imports.map(OWLImportItem) ++ diff.left.annotations.map(OWLObjectItem) ++ diff.left.axioms.map(OWLObjectItem)
    val rightUnique: Set[OWLItem[_]] = diff.right.imports.map(OWLImportItem) ++ diff.right.annotations.map(OWLObjectItem) ++ diff.right.axioms.map(OWLObjectItem)
    (leftUnique, rightUnique)
  }

  private def format(removedLines: Set[String], addedLines: Set[String]): String = {
    import org.geneontology.owl.differ.Util.replaceNewlines
    val removedSorted = removedLines.map(replaceNewlines).map(ax => s"- $ax").toSeq.sorted.mkString("\n")
    val addedSorted = addedLines.map(replaceNewlines).map(ax => s"+ $ax").toSeq.sorted.mkString("\n")
    s"""${removedLines.size} axioms in left ontology but not in right ontology:
$removedSorted${if (removedSorted.nonEmpty) "\n" else ""}
${addedLines.size} axioms in right ontology but not in left ontology:
$addedSorted
"""
  }

  private sealed trait OWLItem[T] {

    def item: T

  }

  private final case class OWLObjectItem(item: OWLObject) extends OWLItem[OWLObject]

  private final case class OWLImportItem(item: OWLImportsDeclaration) extends OWLItem[OWLImportsDeclaration]

}
