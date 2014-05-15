package org.scylo.evo

case class F81G(statA: Double, statC: Double, statG: Double, statT: Double, substitutionRate: Double, alpha: Double) {

  import math._
  import org.scylo.bio.Nuc

  require(abs(statA + statC + statG + statT - 1) <= 1E-10, "F81 parameter statDist has to sum to 1. Found statinary distribution: " +
    s"($statA, $statC, $statG, $statT)")

  /** Array of stationary distribution. Mainly here for optimization. */
  private val stationaryDistribution = Array(statA, statC, statG, statT)

  def statDist(of: Nuc): Double = stationaryDistribution(of.index)

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double =
    if (from == to) {
      pow(1 + (substitutionRate * time / alpha), -alpha) + statDist(from) * (1 - pow(1 + substitutionRate * time / alpha, -alpha))
    } else {
      statDist(to) * (1 - pow(1 + substitutionRate * time / alpha, -alpha))
    }

}
