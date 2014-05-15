package org.scylo.evo

case class K80G(transitionRate: Double, transversionRate: Double, alpha: Double) {

  import math._
  import org.scylo.bio.Nuc

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double = {
    import org.scylo._
    if (from == to) { // match
      0.25 + 0.25 * pow(1 + (4 * transversionRate * time) / alpha, -alpha) +
        0.5 * pow(1 + (2 * (transitionRate + transversionRate) * time) / alpha, -alpha)
    } else if (isTransition(from, to)) { // transition
      0.25 + 0.25 * pow(1 + (4 * transversionRate * time) / alpha, -alpha) -
        0.5 * pow(1 + (2 * (transitionRate + transversionRate) * time) / alpha, -alpha)
    } else { // transversion
      0.25 - 0.25 * pow(1 + (4 * transversionRate * time) / alpha, -alpha)
    }
  }

}
