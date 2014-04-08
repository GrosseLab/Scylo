package org.scylo.evo

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.scylo.bio._

class F81Test extends FunSpec with Matchers {
  describe("The Felsenstein 81 model") {
    it("sums to one") {
      val model = F81( 0.3, 0.2, 0.2, 0.3, 0.05)
      val a = model.substitutionProb(A, A, 10)
      val c = model.substitutionProb(A, C, 10)
      val g = model.substitutionProb(A, G, 10)
      val t = model.substitutionProb(A, T, 10)

      (a + c + g + t) should be( 1.0 +- 1E-10)
    }

    it("is a special case of the TN93 model.") {

      val felsenstein = F81( 0.3, 0.2, 0.2, 0.3, 0.05)
      val aF = felsenstein.substitutionProb(A, A, 10)
      val cF = felsenstein.substitutionProb(A, C, 10)
      val gF = felsenstein.substitutionProb(A, G, 10)
      val tF = felsenstein.substitutionProb(A, T, 10)

      val tamura = TN93( 0.3, 0.2, 0.2, 0.3, 0.05, 0.05, 0.05)
      val aT = tamura.substitutionProb(A, A, 10) 
      val cT = tamura.substitutionProb(A, C, 10) 
      val gT = tamura.substitutionProb(A, G, 10) 
      val tT = tamura.substitutionProb(A, T, 10)

      aF should be(aT +- 1E-10)
      cF should be(cT +- 1E-10)
      gF should be(gT +- 1E-10)
      tF should be(tT +- 1E-10)
      felsenstein.substitutionRate should be( tamura.substitutionRate +- 1E10)
      // ADD CHECKS THAT THE DISTANCE ALSO MATCH

    }
  }
}
