package org.scylo.evo

import org.scylo.bio._

/** Hasegawa M, Kishino H, Yano T. Dating of the human-ape splitting
  * by a molecular clock of mitochondrial DNA. J Mol
  * Evol. 1985;22(2):160-74. PubMed PMID: 3934395.
  */
case class HKY95(statA: Double, statC: Double, statG: Double, statT: Double, transitionRate: Double, transversionRate: Double)
  extends EvoModel[Nuc] {

  private val stationaryDistribution = Array(statA, statC, statG, statT)

  def alphabet: Alphabet[Nuc] = DNA

  def statDist(of: Nuc): Double = stationaryDistribution( of.index )

  def substitutionRate: Double = ???

  def distance( seq1: List[Nuc], seq2: List[Nuc]): Option[Double] = ???

  def substitutionProb( form: Nuc, to: Nuc, time: Double): Double = ???

}
