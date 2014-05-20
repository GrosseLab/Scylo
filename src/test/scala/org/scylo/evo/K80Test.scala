package org.scylo.evo

import org.scalacheck.Gen

import org.scylo.bio._

import org.lanyard.random.KISS

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class K80Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import K80Test._
  import JC69Test.JC69Gen

  describe("The Kimura two-parameter model") {

    it("forms proper probability distributions.") {
      forAll((K80Gen, "Kimura two-parameter model"), (Gen.choose(0.0, maxTime), "time")) {
        (kim: K80, time: Double) =>
        /** Make sure the transition probabilities some to one. */
        DNA.elements.map { to => kim |?| (A, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => kim |?| (C, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => kim |?| (G, to, time) }.sum should be(1.0 +- 1E-15)
        DNA.elements.map { to => kim |?| (T, to, time) }.sum should be(1.0 +- 1E-15)
      }
    }

    it("contains JC69 as a special case.") {
      forAll((JC69Gen, "Jukes-Cantor model"), (Gen.choose(0.0, maxTime), "time")) {
        (jc: JC69, time: Double) =>
          for (from <- DNA.elements; to <- DNA.elements) {
            jc |?| (from, to, time) should be((K80(jc.rate, jc.rate) |?| (from, to, time)) +- 1E-15)
          }
      }
    }
  }

  describe("The Kimura two-parameter model with fixed time") {

    it("computes the same probabilities as K80 with given time.") {
      forAll(
        (K80GenFixed, "Kimura two-parameter model"),
        (Gen.oneOf(A, C, G, T), "from"),
        (Gen.oneOf(A, C, G, T), "to")) { (kimf: K80Fixed, from: Nuc, to: Nuc) =>
          /** Make sure both models compute the same probabilities. */
          kimf |?| (from, to) should be(kimf.unfix |?| (from, to, kimf.time))
        }
    }
  }
}

object K80Test {

  /** Maximal transition rate used for testing. */
  val maxTransitionRate = 20.0

  /** Maximal transversion rate used for testing. */
  val maxTransversionRate = 20.0

  /** K80 generator */
  val K80Gen = for {
    transition <- Gen.choose(0.0, maxTransitionRate)
    transversion <- Gen.choose(0.0, maxTransversionRate)
  } yield K80(transition, transversion)

  /** Maximal time used for testing. */
  val maxTime = 10.0

  /** K80Fixed generator. */
  val K80GenFixed = for {
    kim <- K80Gen
    time <- Gen.choose(0.0, maxTime)
  } yield kim >> time
}
