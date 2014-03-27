package org.scylo.bio

/** Alphabet of a given type. */
trait Alphabet[A] {

  /** All elements of the alphabet. */
  def elements: List[A]

}
