package org.renci.owl.differ

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.blocking

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxOWLObjectRendererImpl
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLNamedObject
import org.semanticweb.owlapi.model.OWLObject
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider
import org.semanticweb.owlapi.util.AxiomSubjectProvider

import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.MediaTypes
import scalaz._
import scalaz.Scalaz._

object Differ {

  private val factory = OWLManager.getOWLDataFactory

  private val rdfsLabel = factory.getRDFSLabel

  private val ImportDummy = IRI.create("http://example.org/owl/import/nothing.owl")

  private object DummyMapper extends OWLOntologyIRIMapper {
    def getDocumentIRI(ontIRI: IRI): IRI = ImportDummy
  }

  final case class ModifiedAxiom(axiom: OWLAxiom, added: Boolean)

  sealed abstract class Grouping

  final case class IRIGrouping(term: OWLClass) extends Grouping

  final case class NonIRIGrouping(obj: OWLObject) extends Grouping

  def loadOntologies(left: IRI, right: IRI, loadImports: Boolean): Future[(OWLOntology, OWLOntology)] = Future {
    val leftManager = OWLManager.createOWLOntologyManager()
    val rightManager = OWLManager.createOWLOntologyManager()
    if (!loadImports) {
      leftManager.createOntology(ImportDummy)
      leftManager.setIRIMappers(Set[OWLOntologyIRIMapper](DummyMapper).asJava)
      rightManager.createOntology(ImportDummy)
      rightManager.setIRIMappers(Set[OWLOntologyIRIMapper](DummyMapper).asJava)
    }
    blocking { (leftManager.loadOntology(left), rightManager.loadOntology(right)) }
  }

  def diff(left: OWLOntology, right: OWLOntology): Diff = {
    val leftAxioms = left.getAxioms(Imports.EXCLUDED).asScala.toSet
    val rightAxioms = right.getAxioms(Imports.EXCLUDED).asScala.toSet
    val removed = leftAxioms -- rightAxioms
    val added = rightAxioms -- leftAxioms
    val allChangedAxioms = removed.map(ModifiedAxiom(_, false)) ++ added.map(ModifiedAxiom(_, true))
    val asp = new AxiomSubjectProvider()
    val grouped = (for {
      ma @ ModifiedAxiom(ax, added) <- allChangedAxioms
      obj = asp.getSubject(ax)
      grouping: Grouping = obj match {
        case named: OWLNamedObject => IRIGrouping(factory.getOWLClass(named.getIRI))
        case iri: IRI              => IRIGrouping(factory.getOWLClass(iri))
        case _                     => NonIRIGrouping(obj)
      }
    } yield Map(grouping -> Set(ma))).reduce(_ |+| _)
    Diff(grouped, right)
  }

  private def render(groups: Map[Grouping, Set[ModifiedAxiom]], renderingOnt: OWLOntology): String = {
    val shortFormProvider = new AnnotationValueShortFormProvider(List(rdfsLabel).asJava, Map.empty.asJava, renderingOnt.getOWLOntologyManager)
    val htmlShortFormProvider = new HTMLShortFormProvider(shortFormProvider)
    val labelRenderer = new ManchesterOWLSyntaxOWLObjectRendererImpl()
    labelRenderer.setShortFormProvider(shortFormProvider)
    val htmlRenderer = new ManchesterOWLSyntaxOWLObjectRendererImpl()
    htmlRenderer.setShortFormProvider(htmlShortFormProvider)
    val sortedGroups = groups.keys.toSeq.sortBy(g => labelRenderer.render(objForGrouping(g)))
    def htmlForAxiom(axiom: ModifiedAxiom) = s"""<li><span class="axiom">${htmlRenderer.render(axiom.axiom)}</span></li>"""
    def changeList(header: String, axioms: Set[ModifiedAxiom]) = s"""
        <h4 class="status-header">$header</h4>
        <ul>
        ${axioms.map(htmlForAxiom).mkString("\n")}
        </ul>
        """
    val content = (for {
      group <- sortedGroups
    } yield {
      val removed = groups(group).filterNot(_.added)
      val added = groups(group).filter(_.added)
      val removedList = if (removed.nonEmpty) changeList("Removed", removed) else ""
      val addedList = if (added.nonEmpty) changeList("Added", added) else ""
      val header = labelRenderer.render(objForGrouping(group))
      s"""<h3 class="frame">$header</h3>\n$removedList\n$addedList"""
    }).mkString("\n\n")
    s"""
<html>
<head>
	<meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
.status-header {
    font-style: italic;
} 
</style>
	<title>OWL diff</title>
</head>
<body>
$content
</body>
      """
  }

  private def objForGrouping(grouping: Grouping): OWLObject = grouping match {
    case IRIGrouping(term)   => term
    case NonIRIGrouping(obj) => obj
  }

  case class Diff(groups: Map[Grouping, Set[ModifiedAxiom]], renderingOnt: OWLOntology)

  object Diff {

    implicit val DiffHTMLMarshaller: ToEntityMarshaller[Diff] = Marshaller.stringMarshaller(MediaTypes.`text/html`).compose { diff =>
      Differ.render(diff.groups, diff.renderingOnt)
    }
  }

}
