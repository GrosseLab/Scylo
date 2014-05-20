package org.scylo.evo

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.scylo.bio._

class F81Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import F81Test._

  describe("The Felsenstein 81 model") {

    it("forms proper probabily distributions.") {
      forAll( (F81Gen, "F81 model"), (Gen.choose(0.0, maxTime), "time") ) { (fels: F81, time: Double) =>
        DNA.elements.map { to => fels |?| (A, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => fels |?| (C, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => fels |?| (G, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => fels |?| (T, to, time) }.sum should be(1.0 +- 1E-15)
      }
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

      aF should be(aT +- 1E-15)
      cF should be(cT +- 1E-15)
      gF should be(gT +- 1E-15)
      tF should be(tT +- 1E-15)
      felsenstein.substitutionRate should be( tamura.substitutionRate +- 1E10)
      // ADD CHECKS THAT THE DISTANCE ALSO MATCH

    } 
  }

    describe("The Felsenstein 81 model") {

      it("computes the same probabilities as F81 with given time.") {
        forAll(
          (F81GenFixed, "Felsenstein 81 model"),
          (Gen.oneOf(A, C, G, T), "from"),
          (Gen.oneOf(A, C, G, T), "to")) { (felsf: F81Fixed, from: Nuc, to: Nuc) =>
          /** Make sure both models compute the same probabilities. */
          felsf |?| (from, to) should be(felsf.unfix |?| (from, to, felsf.time))
        }
      }
    }
}

object F81Test {

  import Gen._

  val maxRate = 20.0

  val F81Gen = for {
    statA <- choose(0.0, 10.0)
    statC <- choose(0.0, 10.0)
    statG <- choose(0.0, 10.0)
    statT <- choose(0.0, 10.0)
    rate <- choose(0.0, maxRate)
    sum = statA + statC + statG + statT
  } yield F81( statA / sum, statC / sum, statG / sum, statT / sum, rate)

  val maxTime = 200.0

  val F81GenFixed = for {
    fels <- F81Gen
    time <- choose(0.0, maxTime)
  } yield fels >> time

}
