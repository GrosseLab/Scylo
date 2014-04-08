package org.scylo.evo

import org.scylo.bio._
import org.scylo._

/**
 * The Kimura two-parameter model.  Source: "Kimura, M. 1980. 'A
 * simple method for estimating evolutionary rates of base
 * substitutions through comparative studies of nucleotide
 * sequences.' J. Mol. Evol. 1980 Dec. 16(2):111-20.
 *
 * @constructor Creates a two-parameter model of Kimura
 * @param transitionRate substitution rate for transitions (often alpha in the literature)
 * @param transversionRate substitution rate for transversion (often beta in the literature)
 */
case class K80(transitionRate: Double, transversionRate: Double) extends EvoModel[Nuc] {

  import math._

  /** The Kimura two-parameter model is defined for the DNA alphabet */
  def alphabet = DNA

  /** Substitution rate */
  def substitutionRate: Double = transitionRate + 2 * transversionRate

  /** The probability of a nucleotide in the stationary distribution. */
  def statDist(of: Nuc): Double = 0.25

  def distance(seq1: List[Nuc], seq2: List[Nuc]): Option[Double] = {
    val (transitions, transversions, length) = countTrans(seq1, seq2)
    val transitProp = transitions.toDouble / length // proportion of transitions
    val transverProp = transversions.toDouble / length // proportion of transitions
    val firstLogArgument = 1 - 2 * transitProp - transverProp
    val secondLogArgument = 1 - 2 * transverProp
    if (firstLogArgument > 0.0 && secondLogArgument > 0.0) // K80 undefined
      Some(-0.5 * log(firstLogArgument) - 0.25 * log(secondLogArgument))
    else
      None
  }

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double = 
    if( from == to ) { // match case
      0.25 + 0.25 * exp(-4 * transversionRate * time) + 0.5 * exp(-2 * (transitionRate + transversionRate) * time)
    } else if( isTransition(from, to) ) { // transition case 
      0.25 + 0.25 * exp(-4 * transversionRate * time) - 0.5 * exp(-2 * (transitionRate + transversionRate) * time)
    } else { // transversion case
      0.25 - 0.25 * exp(-4 * transitionRate * time)
    } 
}
