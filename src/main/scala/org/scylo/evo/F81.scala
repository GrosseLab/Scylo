package org.scylo.evo

import org.scylo.bio._
import org.scylo._
import scala.annotation.switch

/** The Felsenstein model.
  * 
  * @param statA probability of adenine in the stationary distribution
  * @param statC probability of cytosine in the stationary distribution
  * @param statG probability of guanine in the stationary distribution
  * @param statT probability of thymine in the stationary distribution
  * @param substitutionRate substitution rate
  */
case class F81 ( statA: Double, statC: Double, statG: Double, statT: Double, substitutionRate: Double ) extends EvoModel[Nuc] {

  import math._

  require( abs(statA + statC + statG + statT - 1 ) <= 1E-10, "F81 parameter statDist has to sum to 1. Found statinary distribution: " + 
    s"($statA, $statC, $statG, $statT)") 

  /** Array of stationary distribution. Mainly here for optimization. */
  private val stationaryDistribution = Array( statA, statC, statG, statT )

  def alphabet: Alphabet[Nuc] = DNA

  def statDist( of: Nuc ): Double = stationaryDistribution( of.index )

  /** Tajima F, Nei M. Biases of the estimates of DNA divergence
    * obtained by the restriction enzyme technique. J Mol
    * Evol. 1982;18(2):115-20. PubMed PMID: 6284946. 
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

}

object F81 {

  def formArray( statDist: Array[Double], substitutionRate: Double ): F81 = new F81( statDist(0), statDist(1), statDist(2), statDist(3), substitutionRate )

}
