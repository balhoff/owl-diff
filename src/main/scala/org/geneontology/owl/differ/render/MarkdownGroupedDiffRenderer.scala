package org.geneontology.owl.differ.render

import com.google.common.base.Optional
import org.apache.commons.io.output.ByteArrayOutputStream
import org.geneontology.owl.differ.Differ._
import org.geneontology.owl.differ.ManchesterSyntaxOWLObjectRenderer
import org.geneontology.owl.differ.Util.OptionalOption
import org.geneontology.owl.differ.shortform.MarkdownLinkShortFormProvider
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLObjectRenderer
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider

import java.io.{OutputStream, PrintWriter, Writer}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._
import scala.util.Using

object MarkdownGroupedDiffRenderer {

  private val factory = OWLManager.getOWLDataFactory

  private val rdfsLabel = factory.getRDFSLabel

  def renderWriter(diff: GroupedDiff, renderingOntologyProvider: OWLOntologySetProvider, givenWriter: Writer): Unit = {
    Using.resource(new PrintWriter(givenWriter)) { writer =>
      val labelProvider = new AnnotationValueShortFormProvider(List(rdfsLabel).asJava, Map.empty[OWLAnnotationProperty, java.util.List[String]].asJava, renderingOntologyProvider)
      val markdownLinkProvider = new MarkdownLinkShortFormProvider(labelProvider)
      val labelRenderer = new ManchesterSyntaxOWLObjectRenderer()
      labelRenderer.setShortFormProvider(labelProvider)
      val markdownRenderer = new ManchesterSyntaxOWLObjectRenderer()
      markdownRenderer.setShortFormProvider(markdownLinkProvider)
      val groupsToLabels: Map[Grouping, String] = diff.groups.keys.map {
        case group @ IRIGrouping(iri)           => group -> labelRenderer.render(factory.getOWLClass(iri)) // send as OWLClass because labels aren't rendered for IRIs
        case group @ GCIGrouping                => group -> "GCIs"
        case group @ RuleGrouping               => group -> "Rules"
        case group @ NonIRIGrouping(obj)        => group -> labelRenderer.render(obj)
        case group @ OntologyImportGrouping     => group -> "Ontology imports"
        case group @ OntologyAnnotationGrouping => group -> "Ontology annotations"
      }.toMap
      val sortedGroups = OntologyImportGrouping :: OntologyAnnotationGrouping :: (diff.groups - OntologyImportGrouping - OntologyAnnotationGrouping).keys.toList.sortBy(groupsToLabels)
      val header =
        s"""# Ontology comparison

## Left
- Ontology IRI: ${optionalIRI(diff.left.getOntologyIRI)}
- Version IRI: ${optionalIRI(diff.left.getVersionIRI)}
- Loaded from: `${diff.leftSource}`

## Right
- Ontology IRI: ${optionalIRI(diff.right.getOntologyIRI)}
- Version IRI: ${optionalIRI(diff.right.getVersionIRI)}
- Loaded from: `${diff.rightSource}`"""
      writer.println(header)

      for {
        group <- sortedGroups
      } {
        def sortKey(modifiedItem: ModifiedOntologyContent[_]): String = {
          modifiedItem match {
            case ModifiedOntologyAnnotation(item, _) => item.toString
            case ModifiedImport(item, _)             => item.toString
            case ModifiedAxiom(item, _)              => item.getAxiomType match {
              case AxiomType.DECLARATION => s"1-${item.toString}"
              case _                     => s"2-${item.toString}"
            }
          }
        }

        writer.println()
        val removed = diff.groups(group).filterNot(_.added).toSeq.sortBy(sortKey)
        val added = diff.groups(group).filter(_.added).toSeq.sortBy(sortKey)
        val removedList = if (removed.nonEmpty) changeList("Removed", removed.map(_.owlObject).map(item => markdownForObject(item, markdownRenderer))) else ""
        val addedList = if (added.nonEmpty) changeList("Added", added.map(_.owlObject).map(item => markdownForObject(item, markdownRenderer))) else ""
        val header = groupsToLabels(group)
        val headerIRI = group match {
          case IRIGrouping(iri) => Some(iri.toString)
          case _                => None
        }
        val frame = frameElement(header, headerIRI, removedList, addedList)
        writer.println(frame)
      }
    }
  }

  def renderStream(diff: GroupedDiff, renderingOntologyProvider: OWLOntologySetProvider, stream: OutputStream): Unit =
    Using.resource(new PrintWriter(stream)) { writer =>
      renderWriter(diff, renderingOntologyProvider, writer)
    }

  def render(diff: GroupedDiff, renderingOntologyProvider: OWLOntologySetProvider): String =
    Using.resource(new ByteArrayOutputStream()) { stream =>
      renderStream(diff, renderingOntologyProvider, stream)
      stream.toString(StandardCharsets.UTF_8)
    }

  def markdownForObject(obj: OWLObject, renderer: OWLObjectRenderer, level: Int = 0): String = {
    val (annotations, objToRender) = obj match {
      case hasAnnotations: HasAnnotations =>
        val inner = hasAnnotations.getAnnotations.asScala.map(markdownForObject(_, renderer, level + 1)).mkString("\n")
        val objWithoutAnnotations = obj match {
          case ax: OWLAxiom       => ax.getAxiomWithoutAnnotations
          case ann: OWLAnnotation => factory.getOWLAnnotation(ann.getProperty, ann.getValue)
          case _                  => obj
        }
        s"\n$inner" -> objWithoutAnnotations
      case _                              => "" -> obj
    }
    val indentation = "".padTo(level * 2, ' ')
    s"""$indentation- ${renderer.render(objToRender)} $annotations"""
  }

  def changeList(header: String, renderedAxioms: Seq[String]) = s"#### $header\n${renderedAxioms.mkString("\n")}"

  def frameElement(header: String, headerIRI: Option[String], removedList: String, addedList: String): String = {
    val iri = headerIRI.map(iri => s"`$iri`").getOrElse("")
    s"### $header $iri\n$removedList\n$addedList"
  }

  private def optionalIRI(iriOpt: Optional[IRI]): String = iriOpt.toOption.map(iri => s"`$iri`").getOrElse("*None*")

}
