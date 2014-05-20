package org.scylo.evo

import org.lanyard.random.RNG
import org.scylo.bio.Nuc

case class K80Fixed(transitionRate: Double, transversionRate: Double, time: Double) extends EvoModelFixed[Nuc] {

  require( transitionRate >= 0, s"K80Fixed parameter transitionRate needs to be positive. Found value: $transitionRate")
  require( transversionRate >= 0, s"K80Fixed parameter transversionrate needs to be positive. Found value: $transversionRate")
  require( time >= 0, s"K80Fixed parameter time needs to be positive, Found value: $time")

  import math._
  import org.scylo.bio._
  import org.scylo._

  def alphabet = DNA

  private val transitionProbs = { 
    val m = 0.25 + 0.25 * exp(-4 * transversionRate * time) + 0.5 * exp(-2 * (transitionRate + transversionRate) * time) // probability of match 
    val v = 0.25 - 0.25 * exp(-4 * transversionRate * time) // probability of transverion 
    val s = 0.25 + 0.25 * exp(-4 * transversionRate * time) - 0.5 * exp(-2 * (transitionRate + transversionRate) * time) // probability of transition

    Array(
      Array( m, v, s, v ),
      Array( v, m, v, s ),
      Array( s, v, m, v ),
      Array( v, s, v, m ))
  }

  def substitutionProb( from: Nuc, to: Nuc ): Double = transitionProbs(from.index)(to.index)

  def random( source: RNG): ((Nuc, Nuc), RNG) = ???

  def unfix: EvoModel[Nuc] = K80( transitionRate, transversionRate )
}
