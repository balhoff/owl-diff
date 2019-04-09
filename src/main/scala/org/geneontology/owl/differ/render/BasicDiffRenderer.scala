package org.geneontology.owl.differ.render

import org.geneontology.owl.differ.Differ.BasicDiff
import org.geneontology.owl.differ.ShortFormFunctionalSyntaxObjectRenderer
import org.semanticweb.owlapi.model.OWLObject
import org.semanticweb.owlapi.util.ShortFormProvider

object BasicDiffRenderer {

  def renderPlain(diff: BasicDiff): String = {
    if (diff.isEmpty) "Ontologies are identical"
    else {
      val (left, right) = groups(diff)
      format(left.map(_.toString), right.map(_.toString))
    }
  }

  def render(diff: BasicDiff, shortFormProvider: ShortFormProvider): String = {
    if (diff.isEmpty) "Ontologies are identical"
    else {
      val renderer = new ShortFormFunctionalSyntaxObjectRenderer(shortFormProvider)
      val (left, right) = groups(diff)
      format(left.map(renderer.render), right.map(renderer.render))
    }
  }

  private def groups(diff: BasicDiff): (Set[OWLObject], Set[OWLObject]) = {
    val leftUnique: Set[OWLObject] = diff.left.annotations ++ diff.left.axioms
    val rightUnique: Set[OWLObject] = diff.right.annotations ++ diff.right.axioms
    (leftUnique, rightUnique)
  }

  private def format(removedLines: Set[String], addedLines: Set[String]): String = {
    import org.geneontology.owl.differ.Util.replaceNewlines
    val removedSorted = removedLines.map(replaceNewlines).map(ax => s"- $ax").toSeq.sorted.mkString("\n")
    val addedSorted = addedLines.map(replaceNewlines).map(ax => s"+ $ax").toSeq.sorted.mkString("\n")
    s"""${removedLines.size} axioms in Ontology 1 but not in Ontology 2:
$removedSorted

${addedLines.size} axioms in Ontology 2 but not in Ontology 1:
$addedSorted
"""
  }

}
