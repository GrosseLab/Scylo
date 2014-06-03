package org.scylo.evo

import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.scylo.bio._

class TN93Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import TN93Test._

  describe("The Tamura-Nei model") {
    it("forms proper probability distributions.") {
      forAll( (TN93Gen, "TN93 model"), (Gen.choose(0.0, maxTime), "time") ) { (tam: TN93, time: Double) =>
        DNA.elements.map { to => tam |?| (A, to, time) }.sum should be(1.0 +- 1E-14)
        DNA.elements.map { to => tam |?| (C, to, time) }.sum should be(1.0 +- 1E-14)
        DNA.elements.map { to => tam |?| (G, to, time) }.sum should be(1.0 +- 1E-14)
        DNA.elements.map { to => tam |?| (T, to, time) }.sum should be(1.0 +- 1E-14)
      }
    }
  }

  describe("The Tamura-Nei model with fixed time.") {
    it("computes the same probabilities as TN93 with given time") {
      forAll(
        (TN93GenFixed, "Tamura-Nei model"),
        (Gen.oneOf(A, C, G, T), "from"),
        (Gen.oneOf(A, C, G, T), "to")) { (tamf: TN93Fixed, from: Nuc, to: Nuc) =>
        /** Make sure both models compute the same probabilities. */
        tamf |?| (from, to) should be(tamf.unfix |?| (from, to, tamf.time))
      }
    }
  }
}

object TN93Test {

  import Gen._

  val maxTransitionRateAG = 20.0

  val maxTransitionRateCT = 20.0

  val maxTransversionRate = 20.0

  val TN93Gen = for {
    statA <- choose(0.0, 10.0)
    statC <- choose(0.0, 10.0)
    statG <- choose(0.0, 10.0)
    statT <- choose(0.0, 10.0)
    sum = statA + statC + statG + statT
    rateAG <- choose(0.0, maxTransitionRateAG )
    rateCT <- choose(0.0, maxTransitionRateCT)
    rateTransver <- choose(0.0, maxTransversionRate)
  } yield TN93(statA / sum, statC/sum, statG/sum, statT/sum, rateAG, rateCT, rateTransver)

  val maxTime = 200.0

  val TN93GenFixed = for {
    tam <- TN93Gen
    time <- choose(0.0, maxTime)
  } yield tam >> time

}
