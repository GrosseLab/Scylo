package org.scylo.bio

/** Alphabet of a given type. */
trait Alphabet[A] {

  /** All elements of the alphabet. */
  def elements: List[A]

  /** Converts a symbol to an `Int` */
  def toInt( symbol: A ): Int = elements.indexOf( symbol )

}
