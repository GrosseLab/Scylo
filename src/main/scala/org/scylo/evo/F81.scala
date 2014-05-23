package org.scylo.evo

import org.scylo.bio._
import org.scylo._

/** The Felsenstein 81 model.
  * 
  * @constructor Creates a F81 model
  * @param statA stationary distribution of adenine
  * @param statC stationary distribution of cytosine
  * @param statG stationary distribution of guanine 
  * @param statT stationary distribution of thymine
  * @param substitutionRate substitution rate
  */
case class F81 ( statA: Double, statC: Double, statG: Double, statT: Double, substitutionRate: Double ) extends EvoModel[Nuc] {

  import math._

  require( abs(statA + statC + statG + statT - 1 ) <= 1E-10, "F81 parameters statDist have to sum to 1. Found statinary distribution: " + 
    s"($statA, $statC, $statG, $statT)") 
  require( statA >= 0, s"F81 parameter statA needs to be positive, Found value: $statA")
  require( statC >= 0, s"F81 parameter statC needs to be positive, Found value: $statC")
  require( statG >= 0, s"F81 parameter statG needs to be positive, Found value: $statG")
  require( statT >= 0, s"F81 parameter statT needs to be positive, Found value: $statT")

  def this( stat: Array[Double], substitutionRate: Double) = this(stat(0), stat(1), stat(2), stat(3), substitutionRate)

  /** Array of stationary distribution. Mainly here for optimization. */
  private val stationaryDistribution = Array( statA, statC, statG, statT )

  def alphabet: Alphabet[Nuc] = DNA

  def statDist( of: Nuc ): Double = stationaryDistribution( of.index )

  /** 
    * Returns the phylogenetic distance between two sequences.
    * 
    * Source: 'Tajima F, Nei M. Biases of the estimates of DNA divergence
    * obtained by the restriction enzyme technique. J Mol
    * Evol. 1982;18(2):115-20. PubMed PMID: 6284946.'
    */
  def distance( seq1: List[Nuc], seq2: List[Nuc] ): Option[Double] = {
    val tmp = 1.0 - ( statA * statA + statC * statC + statG * statG + statT * statT )
    val (mismatches, length) = countMismatches( seq1, seq2 )

    val logArg  = 1.0 - (mismatches.toDouble / length) / tmp

    if( logArg > 0.0 ) Some(-tmp * log( logArg )) else None
  }

  def substitutionProb(from: Nuc, to: Nuc, time: Double ): Double = 
    if( from == to )
      exp( - substitutionRate * time ) + (1 - exp( -substitutionRate * time)) * statDist(to)
    else
      (1 - exp( -substitutionRate * time)) * statDist(to)

  def >> (time: Double ) = F81Fixed( statA, statC, statG, statT, substitutionRate, time)

}
