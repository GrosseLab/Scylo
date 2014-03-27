package org.scylo.evo

import org.lanyard.random.RNG
import org.lanyard.random.Random
import org.scylo.bio.DNA
import org.scylo.bio.Nuc

/** Source: "Jukes, TH. and Cantor, CR. 1969. 'Evolution of Protein Molecules' New York: Academic Press. pp. 21–132 */
case class JC69(rate: Double) extends EvoModel[Nuc] {

  import math._

  /** Classical JC is defined for the DNA alphabet. */
  def alphabet = DNA

  /** Subsitution rate */
  def substitutionRate = 3 * rate

  /** The probability of a nucleotide in the stationary distribution. */
  def stationaryDistribution(of: Nuc): Double = 0.25

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
    mm.toDouble / length match {
      case p if p >= 0.75 => None
      case p => Some(-0.75 * log(1 - 4.0 / 3 * p))
    }
  }

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double = (from, to) match {
    case (from, to) if from == to => 0.25 + 0.75 * exp(-4 * rate * time)
    case _ => 0.25 - 0.25 * exp(-4 * rate * time)
  }

}
