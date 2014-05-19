package org.scylo.bio

sealed trait Nuc {

  val index: Int

  val complement: Nuc

  def isPyrimidine: Boolean

  @inline def isPurine: Boolean = !isPyrimidine

}

/** Adenin */
case object A extends Nuc {
  val index = 0
  val complement = T
  val isPyrimidine = false
}

/** Cytosine */
case object C extends Nuc {
  val index = 1
  val complement = G
  val isPyrimidine = true
}

/** Guanine */
case object G extends Nuc {
  val index = 2
  val complement = C
  val isPyrimidine = false
}

/** Thymine */
case object T extends Nuc {
  val index = 3
  val complement = A
  val isPyrimidine = true 
}

object DNA extends Alphabet[Nuc] {

  private val nucs = Array(A, C, G, T)

  val elements = List(A, C, G, T)

  def fromInt( index: Int ): Nuc = nucs( index )

}


