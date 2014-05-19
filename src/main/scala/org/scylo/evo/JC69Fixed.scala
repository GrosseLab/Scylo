package org.scylo.evo

import org.lanyard.random.RNG
import org.lanyard.util.Sample
import org.scylo.bio.DNA
import org.scylo.bio.Nuc

// should extend Random[(Nuc, Nuc)]
case class JC69Fixed(rate: Double, time: Double) extends EvoModelFixed[Nuc] {

  import math._

  /** Mainly used to improbe sampling performance. */
  private val probabilities = {
    val m = 0.25 + 0.75 * exp(-4 * rate * time)
    val s = 0.25 - 0.25 * exp(-4 * rate * time)

    Array(
      Array(m, s, s, s),
      Array(s, m, s, s),
      Array(s, s, m, s),
      Array(s, s, s, m))
  }

  def substitutionProb(from: Nuc, to: Nuc): Double = probabilities(from.index)(to.index)

  def random(from: Nuc, source: RNG): (Nuc, RNG) = {
    val (idx, nextRNG ) = Sample.fromArray( probabilities(from.index), source)
    (DNA.fromInt( idx ), nextRNG )
  }

  def random( source: RNG): ((Nuc, Nuc), RNG) = ???
}
