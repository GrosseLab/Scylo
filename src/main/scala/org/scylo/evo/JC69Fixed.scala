package org.scylo.evo

import org.lanyard.random.RNG
import org.lanyard.util.Sample
import org.scylo.bio.{DNA, Nuc}

// should extend Distribution[(Nuc, Nuc)]
case class JC69Fixed(rate: Double, time: Double) extends EvoModelFixed[Nuc] {

  require( rate >= 0, s"JC69Fixed parameter rate needs to be positive. Found value: $rate" )
  require( time >= 0, s"JC69Fixed parameter time needs to be positive. Found value: $time" )

  import math._

  /** Jukes-Cantor is defined over the DNA alphabet. */
  def alphabet = DNA

  /** Computes the transition matrix. */
  private val transitionProbs = {
    val m = 0.25 + 0.75 * exp(-4 * rate * time)
    val s = 0.25 - 0.25 * exp(-4 * rate * time)

    Array(
      Array(m, s, s, s),
      Array(s, m, s, s),
      Array(s, s, m, s),
      Array(s, s, s, m))
  }

  /**
    * Returns the probability of a substitution.
    * 
    * @param from symbol to evolve from
    * @param to symbol to evolve to
    * @return substitution probability
    */
  def substitutionProb(from: Nuc, to: Nuc): Double = transitionProbs(from.index)(to.index)

  def random( source: RNG): ((Nuc, Nuc), RNG) = ???

  def unfix: EvoModel[Nuc] = JC69( rate )

}
