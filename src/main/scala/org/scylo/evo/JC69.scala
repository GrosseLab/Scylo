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
    if( p < 0.75 ) {
      Some(-0.75 * log(1 - 4.0 / 3 * p)) 
    } else None
  }

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double = 
    if( from == to ) {
      0.25 + 0.75 * exp(-4 * rate * time)
    } else {
      0.25 - 0.25 * exp(-4 * rate * time)
    }

}
