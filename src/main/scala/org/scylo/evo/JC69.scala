package org.scylo.evo

import org.scylo.bio.{DNA, Nuc}

/**
 * The Jukes-Cantor model of nucleotide substitutions. It assumes that
 * every nucleotide has the same `rate` of changing into any other
 * nucleotide.
 *
 * Source: "Jukes, TH. and Cantor, CR. 1969. 'Evolution of Protein Molecules' New
 * York: Academic Press. pp. 21–132
 *
 * @constructor Creates a Jukes-Cantor model
 * @param rate rate of change from one nucleotide to another
 */
case class JC69(rate: Double) extends EvoModel[Nuc] {

  require( rate >= 0.0, s"JC69 parameter rate needs to be positive. Found value: $rate")

  import math._

  /** Classical JC is defined for the DNA alphabet. */
  def alphabet = DNA

  /** Subsitution rate */
  def substitutionRate = 3 * rate

  /** The probability of a nucleotide in the stationary distribution. */
  def statDist(of: Nuc): Double = 0.25

  /**
   * Evolutionary distance of two sequences.
   *
   * If the sequences are of unequal length, the distance between the
   * smaller sequence and the prefix of the longer with equal length
   * is used.
   *
   * @param seq1 a sequence
   * @param seq2 a sequence
   * @return evolutionary distance
   */
  def distance(seq1: List[Nuc], seq2: List[Nuc]): Option[Double] = {
    import org.scylo._

    val (mm, length) = countMismatches(seq1, seq2)
    val p = mm.toDouble / length
    if (p < 0.75) {
      Some(-0.75 * log(1 - 4.0 / 3 * p))
    } else None
  }

  /**
   * Computes the probability of a substitution in a given time interval.
   *
   * @param from ancestral nucleotide
   * @param to present nucleotide
   * @param time time to evolve
   */
  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double =
    if (from == to) {
      0.25 + 0.75 * exp(-4 * rate * time)
    } else {
      0.25 - 0.25 * exp(-4 * rate * time)
    }

  /**
   * Fixes the time parameter for the evolutionary process.
   *
   * This is mainly for improved performance if time remains constant
   * for several computations.
   *
   * @param time fixed time
   * @return Jukes-Cantor model with fixed time
   */
  def withTime(t: Double) = new JC69Fixed(rate, t)

  /** Symbolic shortcut for withTime. */
  def >> (t: Double) = withTime(t)

  def gammaRate(alpha: Double): Unit = ???

}

object JC69 {

  /**
   * Returns a Jukes-Cantor model with three times the rate.
   *
   * This method is mainly for convenience. The JC69 model is
   * sometimes introduced with a different definition of the rate
   * parameter. This method converts between the two definitions.
   *
   * @param rate substitution rate of the JC69 model
   * @return JC69 model with given substitution rate
   */
  def withSubstRate(rate: Double): JC69 = JC69(rate / 3)

}
