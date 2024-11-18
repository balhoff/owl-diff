package org.geneontology.owl.differ

import org.geneontology.owl.differ.render.MarkdownGroupedDiffRenderer
import org.semanticweb.owlapi.apibinding.OWLManager
import utest._

import java.io.File

object TestMarkdownSorting extends TestSuite {

  val tests: Tests = Tests {
    val left = OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(new File("src/test/resources/left-1.ofn"))
    val right = OWLManager.createOWLOntologyManager().loadOntologyFromOntologyDocument(new File("src/test/resources/right-1.ofn"))
    test("Things are ordered") {
      val diff = Differ.diff(left, right)
      val grouped = Differ.groupedDiff(diff)
      val output = MarkdownGroupedDiffRenderer.render(grouped, right.getOWLOntologyManager)
      assert(output.indexOf("Class: [class five](http://example.org/E)") <
        output.indexOf("[class five](http://example.org/E) [label](http://www.w3.org/2000/01/rdf-schema#label) \"class five\""))
      assert(output.indexOf("[class five-one](http://example.org/A) [label](http://www.w3.org/2000/01/rdf-schema#label) \"class five-one\"") >
        output.indexOf("[class five-one](http://example.org/A) [definition](http://example.org/1) \"Definition one-five.\""))
      assert(output.indexOf("[class five-one](http://example.org/A) EquivalentTo [class five](http://example.org/E)") >
        output.indexOf("[class five-one](http://example.org/A) [label](http://www.w3.org/2000/01/rdf-schema#label) \"class five-one\""))
      assert(output.indexOf("[class five-one](http://example.org/A) SubClassOf [class three](http://example.org/C)") >
        output.indexOf("[class five-one](http://example.org/A) EquivalentTo [class five](http://example.org/E)"))
    }
  }

}
