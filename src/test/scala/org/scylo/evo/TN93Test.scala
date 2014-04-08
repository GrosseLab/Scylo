package org.scylo.evo

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.scylo.bio._

class TN93Test extends FunSpec with Matchers {
  describe("The Tamura-Nei model") {
    it("sums to one.") {
      val model = TN93( 0.3, 0.2, 0.2, 0.3, 0.05, 0.05, 0.1)
      val a = model.substitutionProb(A, A, 10) 
      val c = model.substitutionProb(A, C, 10) 
      val g = model.substitutionProb(A, G, 10) 
      val t = model.substitutionProb(A, T, 10)

      println(a)
      println(c)
      println(g)
      println(t)

      (a + c + g + t) should be(1.0 +- 1E-10)
    }
  }
}
