package org.geneontology.owl.differ

import org.semanticweb.owlapi.apibinding.OWLManager
import utest._

import java.io.File

object TestDiff extends TestSuite {

  val tests: Tests = Tests {
    val left = OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(new File("src/test/resources/left-1.ofn"))
    val right = OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(new File("src/test/resources/right-1.ofn"))
    test("Number of changed axioms and groups") {
      val diff = Differ.diff(left, right)
      assert(diff.left.axioms.size == 3)
      assert(diff.right.axioms.size == 8)
      val grouped = Differ.groupedDiff(diff)
      assert(grouped.groups.size == 4)
    }
  }

}
