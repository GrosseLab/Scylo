package org.scylo.evo

import org.lanyard.random.RNG
import org.scylo.bio._

/** Models of sequence evolution. */
trait EvoModel[A] {

  /** Alphabet of the symbols. */
  def alphabet: Alphabet[A]

  /** Rate of substituions */
  def substitutionRate: Double

  /** Probability of a symbol in the stationary distribution. */
  def stationaryDistribution( of: A ): Double

  /** Evolutionary distance of two sequences. 
    * 
    * In case the sequences are too divergent, a distance might not be
    * defined in an evolutionary model.
    * 
    * @param seq1 a sequence
    * @param seq2 a sequence
    * @return evolutionary distance
    */
  def distance( seq1: List[A], seq2: List[A] ): Option[Double]

  /** Probability of a substituion in a given time.
    * 
    * @param from symbol to evolve from
    * @param to symbol to evolve to
    * @param time time for evolution
    * @return probability of the substituion 
    */
  def substitutionProb( from: A, to: A, time: Double ): Double

  /** Mutates a symbol.
    * 
    * @param from symbol to evolve from
    * @param time time for evolution
    * @param source source of randomness
    * @return mutated symbol with updated generator
    */
  def mutate( from: A, time: Double, source: RNG ): (A, RNG) = {
    // substitution probs of every symbol
    val probs = alphabet.elements.map { substitutionProb(from, _, time ) } 

    import org.lanyard.util.Sample
    Sample( alphabet.elements, probs, source)
  }

  /** Mutates a list of symbols.
    * 
    * @param symbols list of symbols
    * @param source source of randomness
    * @param time time for evolution
    * @return mutated list with updated generator
    */
  def mutate( symbols: List[A], time: Double, source: RNG ): (List[A], RNG) = {
    symbols.foldRight( (List.empty[A], source) ) { ( elem, acc)  => 
      val (newSymbol, nextRNG) = mutate( elem, time, acc._2 )
      (newSymbol :: acc._1, nextRNG)
    }
  }

}
