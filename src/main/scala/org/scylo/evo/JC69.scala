package org.scylo.evo

import org.lanyard.random.RNG
import org.lanyard.random.Random
import org.scylo.bio.DNA

/** Source: "Jukes, TH. and Cantor, CR. 1969. 'Evolution of Protein Molecules' New York: Academic Press. pp. 21–132 */
case class JC69(rate: Double) {

  /** The probability of a nucleotide in the stationary distribution. */
  def stationaryDistribution(of: DNA): Double = 0.25

  def likelihood(from: DNA, to: DNA, time: Double): Double = JC69(from, to, time, rate)

}

object JC69 {

  import math._

  def apply(from: DNA, to: DNA, time: Double, rate: Double): Double = (from, to) match {
    case (from, to) if from == to => 0.25 + 0.75 * exp(-4 * rate * time)
    case _ => 0.25 - 0.25 * exp(-4 * rate * time)
  }

}
