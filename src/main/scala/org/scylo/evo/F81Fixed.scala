package org.scylo.evo

import org.lanyard.random.RNG
import org.scylo.bio.DNA
import org.scylo.bio.Nuc

/** The Felsenstein 81 model with fixed time.
  * 
  * @constructor Creates a F81 model with fixed time
  * @param statA stationary distribution of adenine
  * @param statC stationary distribution of cytosine
  * @param statG stationary distribution of guanine 
  * @param statT stationary distribution of thymine
  * @param substitutionRate substitution rate
  * @param time time passed in evolution
  */
case class F81Fixed( statA: Double, statC: Double, statG: Double, statT: Double, substitutionRate: Double, time: Double ) extends EvoModelFixed[Nuc] {

  import math._

  require( abs(statA + statC + statG + statT - 1 ) <= 1E-10, "F81Fixed parameters statDist have to sum to 1. Found statinary distribution: " + 
    s"($statA, $statC, $statG, $statT)") 
  require( statA >= 0, s"F81Fixed parameter statA needs to be positive, Found value: $statA")
  require( statC >= 0, s"F81Fixed parameter statC needs to be positive, Found value: $statC")
  require( statG >= 0, s"F81Fixed parameter statG needs to be positive, Found value: $statG")
  require( statT >= 0, s"F81Fixed parameter statT needs to be positive, Found value: $statT")

  def this( stat: Array[Double], substitutionRate: Double, time: Double) = this(stat(0), stat(1), stat(2), stat(3), substitutionRate, time)

  def alphabet = DNA

  private val transitionProbs = {
    val matA = exp( - substitutionRate * time ) + (1 - exp( -substitutionRate * time)) * statA
    val matC = exp( - substitutionRate * time ) + (1 - exp( -substitutionRate * time)) * statC
    val matG = exp( - substitutionRate * time ) + (1 - exp( -substitutionRate * time)) * statG
    val matT = exp( - substitutionRate * time ) + (1 - exp( -substitutionRate * time)) * statT
    val misA = (1 - exp( -substitutionRate * time)) * statA
    val misC = (1 - exp( -substitutionRate * time)) * statC
    val misG = (1 - exp( -substitutionRate * time)) * statG
    val misT = (1 - exp( -substitutionRate * time)) * statT
    Array(
      Array ( matA, misC, misG, misT ),
      Array ( misA, matC, misG, misT ),
      Array ( misA, misC, matG, misT ),
      Array ( misA, misC, misG, matT ))
  }

  def substitutionProb( from: Nuc, to: Nuc ): Double = transitionProbs(from.index)(to.index)

  def random( source: RNG): ((Nuc, Nuc), RNG) = ???

  def unfix: EvoModel[Nuc] = F81( statA, statC, statG, statT, substitutionRate )

}
