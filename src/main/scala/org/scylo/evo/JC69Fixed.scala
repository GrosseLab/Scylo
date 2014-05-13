package org.scylo.evo

import org.scylo.bio.Nuc

// should extend Random[(Nuc, Nuc)]
case class JC69Fixed(rate: Double, time: Double) {

  import math._

  private val matchProb = 0.25 + 0.75 * exp(-4 * rate * time)
  private val mismatchProb = 0.25 - 0.25 * exp(-4 * rate * time)

  def substitutionProb(from: Nuc, to: Nuc): Double =  if( from == to ) matchProb else mismatchProb

}
