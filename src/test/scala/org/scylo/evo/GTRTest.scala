package org.scylo.evo

import org.scalacheck.Gen

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.scylo.bio._

class GTRTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import GTRTest._

  describe("The general time-reversible model") {
    it("forms proper probability distributions.") { 
      forAll((GTRGen, "General time-reversible model"), (Gen.choose(0.0, 100.0), "time")) { (gtr: GTR, time: Double) =>
        /** Make sure the transition probabilities some to one. */
        DNA.elements.map { to => gtr |?| (A, to, time) }.sum should be(1.0 +- 1E-11)
        DNA.elements.map { to => gtr |?| (C, to, time) }.sum should be(1.0 +- 1E-11)
        DNA.elements.map { to => gtr |?| (G, to, time) }.sum should be(1.0 +- 1E-11)
        DNA.elements.map { to => gtr |?| (T, to, time) }.sum should be(1.0 +- 1E-11)
      }
    }
  }
}

object GTRTest {

  import Gen._

  val maxAcRate = 20.0
  val maxAgRate = 20.0
  val maxAtRate = 20.0

  val maxCgRate = 20.0
  val maxCtRate = 20.0

  val GTRGen = for {
    statA <- choose(0.0, 10.0)
    statC <- choose(0.0, 10.0)
    statG <- choose(0.0, 10.0)
    statT <- choose(0.0, 10.0)
    sum = statA + statC + statG + statT
    acRate <- choose(0.0, maxAcRate )
    agRate <- choose(0.0, maxAgRate )
    atRate <- choose(0.0, maxAtRate )
    cgRate <- choose(0.0, maxCgRate )
    ctRate <- choose(0.0, maxCtRate )
  } yield GTR(acRate, agRate, atRate, cgRate, ctRate, statA / sum, statC/sum, statG/sum, statT/sum)

}
