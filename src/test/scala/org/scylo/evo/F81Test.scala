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
      forAll( (F81Gen, "F81 model"), (Gen.choose(0.0, maxTime), "time") ) { (fels: F81, time: Double) =>
        val rate = fels.substitutionRate 
        val tamura = TN93(fels.statA, fels.statC, fels.statG, fels.statT, rate, rate, rate)
        /** The tolerance is set to 1E-14 because the probabilities are
          * calculated using different formulas resulting small
          * floating points errors.*/
        DNA.elements.foreach { to => fels |?| (A, to, time) should be(tamura.substitutionProb(A, to, time) +- 1E-14) }
        DNA.elements.foreach { to => fels |?| (C, to, time) should be(tamura.substitutionProb(C, to, time) +- 1E-14) }
        DNA.elements.foreach { to => fels |?| (G, to, time) should be(tamura.substitutionProb(G, to, time) +- 1E-14) }
        DNA.elements.foreach { to => fels |?| (T, to, time) should be(tamura.substitutionProb(T, to, time) +- 1E-14) }
      }
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
