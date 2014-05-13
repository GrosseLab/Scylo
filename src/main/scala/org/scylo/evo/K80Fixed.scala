package org.scylo.evo

case class K80Fixed(transitionRate: Double, transversionRate: Double, time: Double) {

  import math._
  import org.scylo.bio._
  import org.scylo._

  private val matchProb = 0.25 + 0.25 * exp(-4 * transversionRate * time) + 0.5 * exp(-2 * (transitionRate + transversionRate) * time)
  private val transversionProb = 0.25 - 0.25 * exp(-4 * transitionRate * time)
  private val transitionProb = 0.25 + 0.25 * exp(-4 * transversionRate * time) - 0.5 * exp(-2 * (transitionRate + transversionRate) * time)

  def substitutionProb( from: Nuc, to: Nuc ): Double =
    if( from == to ) matchProb // matches
    else if( isTransition( from, to ) ) transitionProb // transitions
    else transversionProb // transversions

}
