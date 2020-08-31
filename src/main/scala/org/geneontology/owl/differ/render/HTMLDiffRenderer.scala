package org.geneontology.owl.differ.render

import com.google.common.base.Optional
import org.apache.commons.text.StringEscapeUtils
import org.geneontology.owl.differ.Differ._
import org.geneontology.owl.differ.ManchesterSyntaxOWLObjectRenderer
import org.geneontology.owl.differ.Util.OptionalOption
import org.geneontology.owl.differ.shortform.{HTMLLinkShortFormProvider, HTMLSafeIRIShortFormProvider, HTMLSafeShortFormProvider}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.{AnnotationValueShortFormProvider, SimpleIRIShortFormProvider, SimpleShortFormProvider}

import scala.collection.JavaConverters._

object HTMLDiffRenderer {

  private val factory = OWLManager.getOWLDataFactory

  private val rdfsLabel = factory.getRDFSLabel

  def render(diff: GroupedDiff, renderingOntologyProvider: OWLOntologySetProvider): String = {
    val shortFormProvider = new HTMLSafeShortFormProvider(new AnnotationValueShortFormProvider(renderingOntologyProvider, new SimpleShortFormProvider(), new HTMLSafeIRIShortFormProvider(new SimpleIRIShortFormProvider()), List(rdfsLabel).asJava, Map.empty[OWLAnnotationProperty, java.util.List[String]].asJava))
    val htmlLinkShortFormProvider = new HTMLLinkShortFormProvider(shortFormProvider)
    val labelRenderer = new ManchesterSyntaxOWLObjectRenderer()
    labelRenderer.setShortFormProvider(shortFormProvider)
    val htmlRenderer = new ManchesterSyntaxOWLObjectRenderer()
    htmlRenderer.setShortFormProvider(htmlLinkShortFormProvider)
    val groupsToLabels: Map[Grouping, String] = diff.groups.keys.map {
      case group @ IRIGrouping(iri)           => group -> labelRenderer.render(factory.getOWLClass(iri)) // send as OWLClass because labels aren't rendered for IRIs
      case group @ GCIGrouping                => group -> "GCIs"
      case group @ RuleGrouping               => group -> "Rules"
      case group @ NonIRIGrouping(obj)        => group -> labelRenderer.render(obj)
      case group @ OntologyImportGrouping     => group -> "Ontology imports"
      case group @ OntologyAnnotationGrouping => group -> "Ontology annotations"
    }.toMap
    val sortedGroups = OntologyImportGrouping :: OntologyAnnotationGrouping :: (diff.groups - OntologyImportGrouping - OntologyAnnotationGrouping).keys.toList.sortBy(groupsToLabels)

    def htmlForObject(obj: OWLObject): String = {
      val (annotations, objToRender) = obj match {
        case hasAnnotations: HasAnnotations =>
          val inner = hasAnnotations.getAnnotations.asScala.map(htmlForObject(_)).mkString("\n")
          val objWithoutAnnotations = obj match {
            case ax: OWLAxiom       => ax.getAxiomWithoutAnnotations
            case ann: OWLAnnotation => factory.getOWLAnnotation(ann.getProperty, ann.getValue)
            case _                  => obj
          }
          s"<ul>$inner</ul>" -> objWithoutAnnotations
        case _                              => "" -> obj
      }
      s"""<li><span class="axiom">${htmlRenderer.render(objToRender)}</span> $annotations </li>"""
    }

    def changeList(header: String, axioms: Seq[OWLObject]) =
      s"""
        <h4 class="status-header">$header</h4>
        <ul>
        ${axioms.map(htmlForObject).mkString("\n")}
        </ul>
        """

    def frameElement(header: String, headerIRI: String, removedList: String, addedList: String) =
      s"""<div class="frame">
        <h3>$header <span class="frame-iri">$headerIRI</span></h3>
        $removedList
        $addedList
        </div>"""

    val content = (for {
      group <- sortedGroups
    } yield {
      def sortKey(modifiedItem: ModifiedOntologyContent[_]): String = modifiedItem match {
        case ModifiedAnnotation(item, _) => item.toString
        case ModifiedImport(item, _)     => item.toString
        case ModifiedAxiom(item, _)      => item.getAxiomType.getName
      }

      val removed = diff.groups(group).filterNot(_.added).toSeq.sortBy(sortKey)
      val added = diff.groups(group).filter(_.added).toSeq.sortBy(sortKey)
      val removedList = if (removed.nonEmpty) changeList("Removed", removed.map(_.owlObject)) else ""
      val addedList = if (added.nonEmpty) changeList("Added", added.map(_.owlObject)) else ""
      val header = groupsToLabels(group)
      val headerIRI = group match {
        case IRIGrouping(iri) => s"- $iri"
        case _                => ""
      }
      frameElement(header, headerIRI, removedList, addedList)
    }).mkString("\n\n")
    s"""
<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
div.frame {
    margin-top: 2.5em;
}
.status-header {
    font-style: italic;
    margin-top: 0;
    margin-bottom: 0;
}
.frame > ul {
    margin-top: 0.3em;
}
h3 {
    margin-bottom: 0.5em;
}
.frame-iri {
    color: gray;
    font-size: 80%;
}
</style>
	<title>OWL diff</title>
</head>
<body>

<h2>Ontology comparison</h2>
<h3>Left</h3>
<ul>
<li>Ontology IRI: ${optionalIRI(diff.left.getOntologyIRI)}</li>
<li>Version IRI: ${optionalIRI(diff.left.getVersionIRI)}</li>
<li>Loaded from: ${StringEscapeUtils.escapeHtml4(diff.leftSource.toQuotedString)}</li>
</ul>
<h3>Right</h3>
<ul>
<li>Ontology IRI: ${optionalIRI(diff.right.getOntologyIRI)}</li>
<li>Version IRI: ${optionalIRI(diff.right.getVersionIRI)}</li>
<li>Loaded from: ${StringEscapeUtils.escapeHtml4(diff.rightSource.toQuotedString)}</li>
</ul>
$content
</body>
      """
  }

  private def optionalIRI(iriOpt: Optional[IRI]): String = (for {
    iri <- iriOpt.toOption
  } yield {
    StringEscapeUtils.escapeHtml4(iri.toQuotedString)
  }).getOrElse("<i>None</i>")

}
