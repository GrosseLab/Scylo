package org.scylo.evo

import org.scylo.bio._
import org.scylo._

/**
 * Hasegawa M, Kishino H, Yano T. Dating of the human-ape splitting
 * by a molecular clock of mitochondrial DNA. J Mol
 * Evol. 1985;22(2):160-74. PubMed PMID: 3934395.
 *
 * @constructore Creates a HKY model
 * @param transitionRate rate of transitions (often named alpha in the literature)
 * @param transversionRate rate of transversions (often named beta in the literature)
 */
case class HKY85(statA: Double, statC: Double, statG: Double, statT: Double, transitionRate: Double, transversionRate: Double)
  extends EvoModel[Nuc] {

  import math._

  private val stationaryDistribution = Array(statA, statC, statG, statT)

  /** Auxillary variables used for the computation of the transition probabilities */
  private val statR = statA + statG
  private val statY = statC + statT
  private val firstFactorDiag = Array(statG / statR, statT / statY, statA / statR, statC / statY)
  private val secondFactorDiag = Array(statA * statY / statR, statC * statR / statY, statG * statY / statR, statT * statR / statY)
  private val firstFactorOff = Array(1.0 / statR, 1.0 / statY, 1.0 / statR, 1.0 / statY)
  private val secondFactorOff = Array(statY / statR, statR / statY, statY / statR, statR / statY)

  def alphabet: Alphabet[Nuc] = DNA

  def statDist(of: Nuc): Double = stationaryDistribution(of.index)

  def substitutionRate: Double = 2 * transitionRate * (statA * statG + statC * statT) + 2 * transversionRate * statR * statY

  def distance(seq1: List[Nuc], seq2: List[Nuc]): Option[Double] = ???

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double =
    if (from == to) { // match
      if (to.isPyrimidine) {
        statDist(to) + firstFactorDiag(to.index) * exp(time * (-transversionRate - (transitionRate - transversionRate) * statY)) +
          secondFactorDiag(to.index) * exp(-transversionRate * time)
      } else {
        statDist(to) + firstFactorDiag(to.index) * exp(time * (-transitionRate + (transitionRate - transversionRate) * statY)) +
          secondFactorDiag(to.index) * exp(-transversionRate * time)
      }
    } else if (from == A && to == G || from == G && to == A) { // AG transition
      statDist(to) * (1 - firstFactorOff(to.index) * exp(time * (-transitionRate + (transitionRate - transversionRate) * statY)) +
        secondFactorOff(to.index) * exp( -transversionRate * time ))
    } else if (from == T && to == C || from == C && to == T) { // TC transition
      statDist(to) * (1 - firstFactorOff(to.index) * exp(time * (-transversionRate - (transitionRate - transversionRate) * statY)) +
        secondFactorOff(to.index) * exp( -transversionRate * time ))
    } else { // transversion
      statDist(to) * (1 - exp(-transversionRate * time))
    }

  def >> (time: Double) = HKY85Fixed(statA, statC, statG, statT, transitionRate, transversionRate, time )

}
