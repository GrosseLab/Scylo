package org.scylo.evo

import org.lanyard.random.RNG
import org.scylo.bio._

/**
 * Tamura K, Nei M. Estimation of the number of nucleotide substitutions in the
 * control region of mitochondrial DNA in humans and chimpanzees. Mol Biol Evol.
 * 1993 May;10(3):512-26. PubMed PMID: 8336541.
 *
 * @constructor Creates a Tamura-Nei 93 model with fixed time
 * @param transitionRateCT rate of transition between cytosine and thymine (alpha1 in the literature)
 * @param transitionRateAG rate of transition between adenine and guanine (alpha2 in the literature)
 * @param transversionRate rate of transversions (beta in the literature)
 */
case class TN93Fixed(statA: Double, statC: Double, statG: Double, statT: Double, transitionRateAG: Double, transitionRateCT: Double, transversionRate: Double, time: Double)
  extends EvoModelFixed[Nuc] {

  import math._

  private val stationaryDistribution = Array(statA, statC, statG, statT)

  private val transitionProbs = {

    val statR = statA + statG // piR
    val statY = statC + statT // piY

    val e2 = exp(-transversionRate * time)
    val e3 = exp(-(statR * transitionRateAG + statY * transversionRate) * time)
    val e4 = exp(-(statY * transitionRateCT + statR * transversionRate) * time)

    Array(
      Array(
        statA + statA * statY / statR * e2 + statG / statR * e3, // A -> A
        statC * (1 - e2), // A -> C
        statG + statG * statY / statR * e2 - statG / statR * e3, // A -> G
        statT * (1 - e2)), // A -> T
      Array(
        statA * (1 - e2), // C -> A
        statC + statC * statR / statY * e2 + statT / statY * e4, // C -> C
        statG * (1 - e2), // C -> G
        statT + statT * statR / statY * e2 - statT / statY * e4), // C -> T
      Array(
        statA + statA * statY / statR * e2 - statA/statR * e3, // G -> A
        statC * ( 1 - e2), // G -> C
        statG + statG * statY / statR * e2 + statA/statR * e3, // G -> G
        statT * ( 1 - e2)), // G -> T
      Array(
        statA * (1 - e2), // T -> A
        statC + statC * statR / statY * e2 - statC / statY * e4, // T -> C
        statG * (1 - e2), // T -> G
        statT + statT * statR / statY * e2 + statC / statY * e4 // T -> T
        ))
  }

  def alphabet = DNA

  def substitutionProb(from: Nuc, to: Nuc): Double = transitionProbs(from.index)(to.index)

  def random( source: RNG ): ((Nuc, Nuc), RNG) = ???

  def unfix = TN93(statA, statC, statG, statT, transitionRateAG, transitionRateCT, transversionRate)

}
