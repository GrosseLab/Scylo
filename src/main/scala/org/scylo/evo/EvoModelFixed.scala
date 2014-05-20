package org.scylo.evo

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG
import org.lanyard.random.Random
import org.lanyard.util.Sample
import org.scylo.bio.Alphabet

/** Evolutionary model of fixed time. These models can be derived from
  * any classical evolutionary model and are only for optimization. */
trait EvoModelFixed[A] extends Distribution[(A, A)] {

  /** Alphabet overy which this EvoModel is defined. */
  def alphabet: Alphabet[A]

  /**
   * Returns the probability of a substitution.
   *
   * @param from symbol from which to evolve
   * @param to symbol to evolve to
   * @return probability of the substitution
   */
  def substitutionProb(from: A, to: A): Double

  /** Symbolic alias for `substitutionProb` */
  def |?|(from: A, to: A): Double = substitutionProb(from, to)

  /**
   * Provides a `Random` instance to model a substitution from a given symbol.
   *
   * @param from symbol to evolve from
   * @return new nucleotide wrapped in a `Random`
   */
  def mutate(from: A): Random[A] = new Random[A] {
    def random(source: RNG): (A, RNG) = {
      val weights = alphabet.elements.map { substitutionProb(from, _) }
      Sample(alphabet.elements, weights, source)
    }
  }

  /** Symbolic alias for `given` */
  def |!|(from: A) = mutate(from)

  /** Drops the constraint of a fixed time. */
  def unfix: EvoModel[A]

}
