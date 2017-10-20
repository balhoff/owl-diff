package org.renci.owl.differ

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.blocking

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.HasAnnotations
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLImportsDeclaration
import org.semanticweb.owlapi.model.OWLNamedObject
import org.semanticweb.owlapi.model.OWLObject
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyID
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper
import org.semanticweb.owlapi.model.SWRLObject
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider
import org.semanticweb.owlapi.util.AxiomSubjectProviderEx
import org.semanticweb.owlapi.util.SimpleIRIShortFormProvider
import org.semanticweb.owlapi.util.SimpleShortFormProvider

import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.MediaTypes
import scalaz._
import scalaz.Scalaz._
import org.apache.commons.text.StringEscapeUtils
import java.util.Optional

object Differ {

  private val factory = OWLManager.getOWLDataFactory

  private val rdfsLabel = factory.getRDFSLabel()

  private val ImportDummy = IRI.create("http://example.org/owl/import/nothing.owl")

  private object DummyMapper extends OWLOntologyIRIMapper {
    def getDocumentIRI(ontIRI: IRI): IRI = ImportDummy
  }

  final case class ModifiedAxiom(axiom: OWLAxiom, added: Boolean)

  sealed abstract class Grouping

  final case class IRIGrouping(term: OWLClass) extends Grouping

  final case class NonIRIGrouping(obj: OWLObject) extends Grouping

  object GCIGrouping extends Grouping

  object RuleGrouping extends Grouping

  def loadOntologies(left: IRI, right: IRI, loadImports: Boolean): Future[(OWLOntology, OWLOntology)] = {
    val leftManager = OWLManager.createOWLOntologyManager()
    val rightManager = OWLManager.createOWLOntologyManager()
    if (!loadImports) {
      leftManager.createOntology(ImportDummy)
      leftManager.setIRIMappers(Set[OWLOntologyIRIMapper](DummyMapper).asJava)
      rightManager.createOntology(ImportDummy)
      rightManager.setIRIMappers(Set[OWLOntologyIRIMapper](DummyMapper).asJava)
    }
    val leftOntologyFut = Future { blocking { leftManager.loadOntology(left) } }
    val rightOntologyFut = Future { blocking { rightManager.loadOntology(right) } }
    for {
      leftOnt <- leftOntologyFut
      rightOnt <- rightOntologyFut
    } yield (leftOnt, rightOnt)
  }

  def diff(left: OWLOntology, right: OWLOntology): Diff = {
    val leftAxioms = left.getAxioms(Imports.EXCLUDED).asScala.toSet
    val rightAxioms = right.getAxioms(Imports.EXCLUDED).asScala.toSet
    val removed = leftAxioms -- rightAxioms
    val added = rightAxioms -- leftAxioms
    val allChangedAxioms = removed.map(ModifiedAxiom(_, false)) ++ added.map(ModifiedAxiom(_, true))
    val groupBySubject = for {
      ma @ ModifiedAxiom(ax, added) <- allChangedAxioms
      obj = AxiomSubjectProviderEx.getSubject(ax)
      grouping: Grouping = obj match {
        case named: OWLNamedObject   => IRIGrouping(factory.getOWLClass(named.getIRI))
        case iri: IRI                => IRIGrouping(factory.getOWLClass(iri))
        case gci: OWLClassExpression => GCIGrouping
        case swrl: SWRLObject        => RuleGrouping
        case _                       => NonIRIGrouping(obj)
      }
    } yield Map(grouping -> Set(ma))
    val grouped = if (groupBySubject.nonEmpty) groupBySubject.reduce(_ |+| _) else Map.empty[Grouping, Set[ModifiedAxiom]]
    val leftOntAnnotations = left.getAnnotations().asScala.toSet
    val rightOntAnnotations = right.getAnnotations().asScala.toSet
    val removedAnnotations = leftOntAnnotations -- rightOntAnnotations
    val addedAnnotations = rightOntAnnotations -- leftOntAnnotations
    val leftImports = left.getImportsDeclarations.asScala.toSet
    val rightImports = right.getImportsDeclarations.asScala.toSet
    val removedImports = leftImports -- rightImports
    val addedImports = rightImports -- leftImports
    Diff(left.getOntologyID, left.getOWLOntologyManager.getOntologyDocumentIRI(left), right.getOntologyID, right.getOWLOntologyManager.getOntologyDocumentIRI(right), grouped, removedImports, addedImports, removedAnnotations, addedAnnotations, right)
  }

  def render(diff: Diff): String = {
    val shortFormProvider = new HTMLShortFormProvider(new AnnotationValueShortFormProvider(diff.renderingOnt.getOWLOntologyManager, new SimpleShortFormProvider(), new HTMLSafeIRIShortFormProvider(new SimpleIRIShortFormProvider()), List(rdfsLabel).asJava, Map.empty.asJava))
    val htmlLinkShortFormProvider = new HTMLLinkShortFormProvider(shortFormProvider)
    val labelRenderer = new ManchesterSyntaxOWLObjectRenderer()
    labelRenderer.setShortFormProvider(shortFormProvider)
    val htmlRenderer = new ManchesterSyntaxOWLObjectRenderer()
    htmlRenderer.setShortFormProvider(htmlLinkShortFormProvider)
    val groupsToLabels: Map[Grouping, String] = (diff.groups.keys.map { group =>
      group match {
        case IRIGrouping(term)   => group -> labelRenderer.render(term)
        case GCIGrouping         => group -> "GCIs"
        case RuleGrouping        => group -> "Rules"
        case NonIRIGrouping(obj) => group -> labelRenderer.render(obj)
      }
    }).toMap
    val sortedGroups = diff.groups.keys.toSeq.sortBy(groupsToLabels)
    def htmlForObject(obj: OWLObject): String = {
      val (annotations, objToRender) = if (obj.isInstanceOf[HasAnnotations]) {
        val inner = obj.asInstanceOf[HasAnnotations].getAnnotations().asScala.map(htmlForObject(_)).mkString("\n")
        val objWithoutAnnotations = obj match {
          case ax: OWLAxiom       => ax.getAxiomWithoutAnnotations()
          case ann: OWLAnnotation => factory.getOWLAnnotation(ann.getProperty, ann.getValue)
          case _                  => obj
        }
        s"<ul>$inner</ul>" -> objWithoutAnnotations
      } else "" -> obj
      s"""<li><span class="axiom">${htmlRenderer.render(objToRender)}</span> $annotations </li>"""
    }
    def changeList(header: String, axioms: Seq[OWLObject]) = s"""
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
      val removed = diff.groups(group).filterNot(_.added).toSeq.sortBy(_.axiom.getAxiomType.getName)
      val added = diff.groups(group).filter(_.added).toSeq.sortBy(_.axiom.getAxiomType.getName)
      val removedList = if (removed.nonEmpty) changeList("Removed", removed.map(_.axiom)) else ""
      val addedList = if (added.nonEmpty) changeList("Added", added.map(_.axiom)) else ""
      val header = groupsToLabels(group)
      val headerIRI = group match {
        case IRIGrouping(term) => s"- ${term.getIRI}"
        case _                 => ""
      }
      frameElement(header, headerIRI, removedList, addedList)
    }).mkString("\n\n")
    val importsContent = if (diff.removedImports.nonEmpty || diff.addedImports.nonEmpty) {
      val removedImportsList = if (diff.removedImports.nonEmpty) changeList("Removed", diff.removedImports.map(_.getIRI).toSeq) else ""
      val addedImportsList = if (diff.addedImports.nonEmpty) changeList("Added", diff.addedImports.map(_.getIRI).toSeq) else ""
      frameElement("Ontology imports", "", removedImportsList, addedImportsList)
    } else ""
    val annotationsContent = if (diff.removedAnnotations.nonEmpty || diff.addedAnnotations.nonEmpty) {
      val removedAnnotationsList = if (diff.removedAnnotations.nonEmpty) changeList("Removed", diff.removedAnnotations.toSeq) else ""
      val addedAnnotationsList = if (diff.addedAnnotations.nonEmpty) changeList("Added", diff.addedAnnotations.toSeq) else ""
      frameElement("Ontology annotations", "", removedAnnotationsList, addedAnnotationsList)
    } else ""
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
<h3>New</h3>
<ul>
<li>Ontology IRI: ${optionalIRI(diff.rightID.getOntologyIRI)}</li>
<li>Version IRI: ${optionalIRI(diff.rightID.getVersionIRI)}</li>
<li>Loaded from: ${StringEscapeUtils.escapeHtml4(diff.rightSource.toQuotedString)}</li>
</ul>
<h3>Old</h3>
<ul>
<li>Ontology IRI: ${optionalIRI(diff.leftID.getOntologyIRI)}</li>
<li>Version IRI: ${optionalIRI(diff.leftID.getVersionIRI)}</li>
<li>Loaded from: ${StringEscapeUtils.escapeHtml4(diff.leftSource.toQuotedString)}</li>
</ul>
$importsContent
$annotationsContent
$content
</body>
      """
  }

  private def optionalIRI(iriOpt: Optional[IRI]): String = (for {
    iri <- iriOpt.toOption
  } yield {
    StringEscapeUtils.escapeHtml4(iri.toQuotedString)
  }).getOrElse("<i>None</i>")

  case class Diff(
    leftID: OWLOntologyID,
    leftSource: IRI,
    rightID: OWLOntologyID,
    rightSource: IRI,
    groups: Map[Grouping, Set[ModifiedAxiom]],
    removedImports: Set[OWLImportsDeclaration],
    addedImports: Set[OWLImportsDeclaration],
    removedAnnotations: Set[OWLAnnotation],
    addedAnnotations: Set[OWLAnnotation],
    renderingOnt: OWLOntology)

  object Diff {

    implicit val DiffHTMLMarshaller: ToEntityMarshaller[Diff] = Marshaller.stringMarshaller(MediaTypes.`text/html`).compose(Differ.render)

  }

  implicit class OptionalOption[T](val self: Optional[T]) extends AnyVal {

    def toOption: Option[T] = if (self.isPresent) Option(self.get) else None

  }

}
