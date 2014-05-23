package org.scylo.evo

import org.scylo.bio._

/**
 * Tamura K, Nei M. Estimation of the number of nucleotide substitutions in the
 * control region of mitochondrial DNA in humans and chimpanzees. Mol Biol Evol.
 * 1993 May;10(3):512-26. PubMed PMID: 8336541.
 *
 * @param transitionRateAG rate of transition between adenine and guanine
 * @param transitionRateCT rate of transition between cytosine and thymine
 * @param transversionRate rate of transversions
 */
case class TN93(statA: Double, statC: Double, statG: Double, statT: Double, transitionRateAG: Double, transitionRateCT: Double, transversionRate: Double)
  extends EvoModel[Nuc] {

  import math._

  private val stationaryDistribution = Array(statA, statC, statG, statT)

  /** Auxillary variables often used in the computation of distance and likelihood. */
  private val multTC = 2 * statT * statC
  private val multAG = 2 * statA * statG
  private val plusTC = statT + statC
  private val plusAG = statA + statG

  /** Auxillary variavles often used in the transition matrix. */
  private val firstFactorDiag = plusTC / plusAG
  private val secondFactorDiag = Array(statG / plusAG, statT / plusTC, statA / plusAG, statC / plusTC)

  def alphabet: Alphabet[Nuc] = DNA

  def statDist(of: Nuc): Double = stationaryDistribution(of.index)

  def substitutionRate: Double = {
    multTC * transitionRateCT +
      multAG * transitionRateAG +
      2 * plusTC * plusAG * transversionRate
  }

  def distance(seq1: List[Nuc], seq2: List[Nuc]): Option[Double] = {
    import org.scylo._
    val (transitAG, transitTC, transver, length) = countTrans2(seq1, seq2)
    val transitAgProp = transitAG.toDouble / length
    val transitTcProp = transitTC.toDouble / length
    val transverProp = transver.toDouble / length
    val logArg1 = 1.0 - (plusTC * transitTcProp) / multTC - transverProp / (2 * plusTC)
    val logArg2 = 1.0 - (plusAG * transitAgProp) / multAG - transverProp / (2 * plusAG)
    val logArg3 = 1.0 - transverProp / (2 * plusTC * plusAG)
    if (logArg1 > 0 && logArg2 > 0 && logArg3 > 0) {
      val a1 = -log(logArg1)
      val a2 = -log(logArg2)
      val b = -log(logArg3)
      Some(multTC / plusTC * (a1 - plusAG * b) + multAG / plusAG * (a2 - plusTC * b) + 2 * plusTC * plusAG * b)
    } else None
  }

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double =
    if (from == to) { // match case
      if (from.isPyrimidine) {
        statDist(to) +
          statDist(to) / firstFactorDiag * exp(-transversionRate * time) +
          secondFactorDiag(from.index) * exp(-(plusTC * transitionRateCT + plusAG * transversionRate) * time)
      } else {
        statDist(to) +
          statDist(to) * firstFactorDiag * exp(-transversionRate * time) +
          secondFactorDiag(from.index) * exp(-(plusAG * transitionRateAG + plusTC * transversionRate) * time)
      }
    } else if (from == A && to == G || from == G && to == A) { // AG transition case
      statDist(to) +
        statDist(to) * firstFactorDiag * exp(-transversionRate * time) -
        statDist(to) / plusAG * exp(-(plusAG * transitionRateAG + plusTC * transversionRate) * time)
    } else if (from == T && to == C || from == C && to == T) { // TC transition case
      statDist(to) +
        statDist(to) / firstFactorDiag * exp(-transversionRate * time) -
        statDist(to) / plusTC * exp(-(plusTC * transitionRateCT + plusAG * transversionRate) * time)
    } else { // transversion case
      statDist(to) * (1.0 - exp(-transversionRate * time))
    }

  def >> (time: Double) = TN93Fixed(statA, statC, statG, statT, transitionRateAG, transitionRateCT, transversionRate, time)
}
