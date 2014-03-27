package org.scylo.bio

sealed trait Nuc {

  def isPyrimidine: Boolean

  def isPurine: Boolean = !isPyrimidine

}

/** Adenin */
case object A extends Nuc {
  val isPyrimidine = false
}

/** Cytosine */
case object C extends Nuc {
  val isPyrimidine = true
}

/** Guanine */
case object G extends Nuc {
  val isPyrimidine = false
}

/** Thymine */
case object T extends Nuc {
  val isPyrimidine = true 
}

object DNA extends Alphabet[Nuc] {

  val elements = List(A, C, G, T)

}


