package org.scylo.evo

import org.scalacheck.Gen

import org.scylo.bio._

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class HKY85Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import HKY85Test._

  describe("The HKY model") {

    it("forms proper probability distributions.") {
      forAll((HKY85Gen, "HKY model"), (Gen.choose(0.0, 200.0), "time")) { (hky: HKY85, time: Double) =>
        /** Make sure the transition probabilities some to one. */
        DNA.elements.map { to => hky |?| (A, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => hky |?| (C, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => hky |?| (G, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => hky |?| (T, to, time) }.sum should be(1.0 +- 1E-15)
      }
    }
  }

  describe("The HKY model with fixed time.") {

    it("computes the same probabilities as HKY with given time.") {
      forAll(
        (HKY85GenFixed, "HKY model"),
        (Gen.oneOf(A, C, G, T), "from"),
        (Gen.oneOf(A, C, G, T), "to")) { (hase: HKY85Fixed, from: Nuc, to: Nuc) =>
        /** Make sure both models compute the same probabilities. */
        hase |?| (from, to) should be(hase.unfix |?| (from, to, hase.time))
      }
    }
  }
}

object HKY85Test {

  import Gen._

  val maxTransitionRate = 20.0

  val maxTransversionRate = 20.0

  val HKY85Gen = for {
    statA <- choose(0.0, 10.0)
    statC <- choose(0.0, 10.0)
    statG <- choose(0.0, 10.0)
    statT <- choose(0.0, 10.0)
    sum = statA + statC + statG + statT
    rateTransit <- choose(0.0, maxTransitionRate )
    rateTransver <- choose(0.0, maxTransversionRate)
  } yield HKY85(statA / sum, statC/sum, statG/sum, statT/sum, rateTransit, rateTransver)

  val maxTime = 200.0

  val HKY85GenFixed = for {
    hase <- HKY85Gen
    time <- choose(0.0, maxTime)
  } yield hase >> time


}
