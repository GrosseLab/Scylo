package org.scylo.evo

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scylo.bio._

class GTRTest extends FunSpec with Matchers {

  describe("The general time-reversible model") {
    it("has the correct rate matrix.") {
      /** This example is taken from Yang (1994b). */
      val piT = 0.308
      val piC = 0.185
      val piG = 0.199
      val piA = 1 - (piC + piG + piT)
      val rateAC = 1.0 // d 0.243
      val rateAG = 1.0 // f 1.0
      val rateAT = 1.0 // b 0.11
      val rateCG = 1.0 // e 0.395
      val rateCT = 1.0 // a 0.987
      val rateGT = 1.0 // c 0.218

      val model = GTR(rateAC, rateAG, rateAT, rateCG, rateCT, piA, piC, piG, piT)

      val probs = Array(
      model.substitutionProb(A, A, 0.1),
      model.substitutionProb(A, C, 0.1),
      model.substitutionProb(A, G, 0.1),
      model.substitutionProb(A, T, 0.1))

      println( probs.mkString(" ") )
      println( probs.sum )

    }
  }

}
