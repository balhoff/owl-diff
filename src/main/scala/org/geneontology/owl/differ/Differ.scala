package org.geneontology.owl.differ

import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.AxiomSubjectProvider

import scala.jdk.CollectionConverters._

object Differ {

  private val AxiomSubjectProviderInst = new AxiomSubjectProvider()

  sealed trait ModifiedOntologyContent[A] extends Product with Serializable {

    def item: A

    def added: Boolean

    def owlObject: OWLObject

  }

  final case class ModifiedAxiom(item: OWLAxiom, added: Boolean) extends ModifiedOntologyContent[OWLAxiom] {

    def owlObject: OWLObject = item

  }

  final case class ModifiedImport(item: OWLImportsDeclaration, added: Boolean) extends ModifiedOntologyContent[OWLImportsDeclaration] {

    def owlObject: OWLObject = item.getIRI

  }

  final case class ModifiedOntologyAnnotation(item: OWLAnnotation, added: Boolean) extends ModifiedOntologyContent[OWLAnnotation] {

    def owlObject: OWLObject = item

  }

  sealed trait Grouping extends Product with Serializable

  final case class IRIGrouping(term: IRI) extends Grouping

  final case class NonIRIGrouping(obj: OWLObject) extends Grouping

  case object GCIGrouping extends Grouping

  case object RuleGrouping extends Grouping

  case object OntologyImportGrouping extends Grouping

  case object OntologyAnnotationGrouping extends Grouping

  def diff(left: OWLOntology, right: OWLOntology): BasicDiff = {
    val leftAxioms = left.getAxioms(Imports.EXCLUDED).asScala.toSet
    val rightAxioms = right.getAxioms(Imports.EXCLUDED).asScala.toSet
    val leftUniqueAxioms = leftAxioms -- rightAxioms
    val rightUniqueAxioms = rightAxioms -- leftAxioms
    val leftOntAnnotations = left.getAnnotations.asScala.toSet
    val rightOntAnnotations = right.getAnnotations.asScala.toSet
    val leftUniqueAnnotations = leftOntAnnotations -- rightOntAnnotations
    val rightUniqueAnnotations = rightOntAnnotations -- leftOntAnnotations
    val leftImports = left.getImportsDeclarations.asScala.toSet
    val rightImports = right.getImportsDeclarations.asScala.toSet
    val leftUniqueImports = leftImports -- rightImports
    val rightUniqueImports = rightImports -- leftImports
    val leftUnique = OntologyContent(left.getOntologyID, left.getOWLOntologyManager.getOntologyDocumentIRI(left), leftUniqueImports, leftUniqueAnnotations, leftUniqueAxioms)
    val rightUnique = OntologyContent(right.getOntologyID, right.getOWLOntologyManager.getOntologyDocumentIRI(right), rightUniqueImports, rightUniqueAnnotations, rightUniqueAxioms)
    BasicDiff(leftUnique, rightUnique)
  }

  def groupedDiff(diff: BasicDiff): GroupedDiff = {
    val allChangedAxioms: Set[ModifiedOntologyContent[_]] = diff.left.axioms.map(ModifiedAxiom(_, false)) ++ diff.right.axioms.map(ModifiedAxiom(_, true))
    val allChangedImports: Set[ModifiedOntologyContent[_]] = diff.left.imports.map(ModifiedImport(_, false)) ++ diff.right.imports.map(ModifiedImport(_, true))
    val allChangedAnnotations: Set[ModifiedOntologyContent[_]] = diff.left.annotations.map(ModifiedOntologyAnnotation(_, false)) ++ diff.right.annotations.map(ModifiedOntologyAnnotation(_, true))
    val groupedAxioms = allChangedAxioms.groupBy {
      case ModifiedAxiom(ax, _)             =>
        AxiomSubjectProviderInst.getSubject(ax) match {
          case named: OWLNamedObject => IRIGrouping(named.getIRI)
          case iri: IRI              => IRIGrouping(iri)
          case _: OWLClassExpression => GCIGrouping
          case _: SWRLObject         => RuleGrouping
          case subj                  => NonIRIGrouping(subj)
        }
      case ModifiedOntologyAnnotation(_, _) => OntologyAnnotationGrouping //shouldn't be matched
      case ModifiedImport(_, _)             => OntologyImportGrouping //shouldn't be matched
    }
    val allGrouped = groupedAxioms + (OntologyImportGrouping -> allChangedImports) + (OntologyAnnotationGrouping -> allChangedAnnotations)
    GroupedDiff(diff.left.id, diff.left.source, diff.right.id, diff.right.source, allGrouped)
  }

  final case class OntologyContent(id: OWLOntologyID, source: IRI, imports: Set[OWLImportsDeclaration], annotations: Set[OWLAnnotation], axioms: Set[OWLAxiom]) {

    def isEmpty: Boolean = imports.isEmpty && annotations.isEmpty && axioms.isEmpty

  }

  final case class BasicDiff(left: OntologyContent, right: OntologyContent) {

    def isEmpty: Boolean = ((left.id == right.id) || (left.id.isAnonymous && right.id.isAnonymous)) && left.isEmpty && right.isEmpty

  }

  final case class GroupedDiff(left: OWLOntologyID, leftSource: IRI, right: OWLOntologyID, rightSource: IRI, groups: Map[Grouping, Set[ModifiedOntologyContent[_]]])

}
