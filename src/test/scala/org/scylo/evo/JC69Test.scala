package org.scylo.evo

import org.scalacheck.Gen

import org.scylo.bio._

import org.lanyard.random.KISS

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class JC69Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import JC69Test._

  describe("The Jukes-Cantor model") {

    it("forms proper probability distributions.") {
      forAll((JC69Gen, "Jukes-Cantor model"), (Gen.choose(0.0, 200.0), "time")) { (jc: JC69, time: Double) =>
        /** Make sure the transition probabilities some to one. */
        for (from <- DNA.elements) {
          DNA.elements.map { to => jc |?| (from, to, time) }.sum should be(1.0 +- 1E-15)
        }
      }
    }
  }

  describe("The Jukes-Cantor model with fixed time") {

    it("computes the same probabilities as JC69 with given time.") {
      forAll(
        (JC69GenFixed, "Fixed-time Jukes-Cantor model"),
        (Gen.oneOf(A, C, G, T), "from"),
        (Gen.oneOf(A, C, G, T), "to")) { (jcf: JC69Fixed, from: Nuc, to: Nuc) =>
            /** Make sure both models compute the same probabilities. */
            jcf |?| (from, to) should be(jcf.unfix |?| (from, to, jcf.time))
      }
    }
  }
}

object JC69Test {

  /** Maximal tested rate of substitution. */
  val maxRate = 20.0

  /** Generator for Jukes-Cantor models. */
  val JC69Gen = for {
    rate <- Gen.choose(0.0, maxRate)
  } yield JC69(rate)

  /** Maximal tested time of evolution. */
  val maxTime = 200.0

  /** Generator for fixed time Jukes-Cantor models. */
  val JC69GenFixed = for {
    jc <- JC69Gen
    time <- Gen.choose(0.0, maxTime)
  } yield jc >> time

}
