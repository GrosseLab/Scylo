package org.scylo.evo

import org.lanyard.random.RNG
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
case class HKY85Fixed(statA: Double, statC: Double, statG: Double, statT: Double, transitionRate: Double, transversionRate: Double, time: Double)
  extends EvoModelFixed[Nuc] {

  import math._

  private val transitionProbs = {

    val statY = statC + statT
    val statR = statA + statG

    val e2 = exp(-transversionRate * time)
    val e3 = exp(time * (-transversionRate - (transitionRate - transversionRate) * statY))
    val e4 = exp(time * (-transitionRate + (transitionRate - transversionRate) * statY))

    Array(
      Array(
        statA + statG / statR * e4 + statA * statY / statR * e2, // A -> A
        (1 - e2) * statC, // A -> C
        statG * (1 - 1.0 / statR * e4 + statY / statR * e2), // A -> G
        (1 - e2) * statT), // A -> T
      Array(
        (1 - e2) * statA, // C -> A
        statC + statT / statY * e3 + statC * statR / statY * e2, // C -> C
        (1 - e2) * statG, // C -> G
        statT * (1 - 1.0 / statY * e3 + statR / statY * e2)), // C -> T
      Array(
        statA * (1 - 1.0 / statR * e4 + statY / statR * e2), // G -> A
        (1 - e2) * statC, // G -> C
        statG + statA / statR * e4 + statG * statY / statR * e2, // G -> G
        (1 - e2) * statT), // G -> T
      Array(
        (1 - e2) * statA, // T -> A
        statC * (1 - 1.0 / statY * e3 + statR / statY * e2), // T -> C
        (1 - e2) * statG, // T -> G
        statT + statC / statY * e3 + statT * statR / statY * e2 // T -> T
        ))
  }

  def alphabet = DNA

  def substitutionProb(from: Nuc, to: Nuc): Double = transitionProbs(from.index)(to.index)

  def random( source: RNG ): ((Nuc, Nuc), RNG) = ???

  def unfix = HKY85(statA, statC, statG, statT, transitionRate, transversionRate)
}
