package org.scylo.bio

sealed trait Nuc {

  def isPyrimidine: Boolean

  def isPurine: Boolean = !isPyrimidine

  val index: Int

}

/** Adenin */
case object A extends Nuc {
  val index = 0
  val isPyrimidine = false
}

/** Cytosine */
case object C extends Nuc {
  val index = 1
  val isPyrimidine = true
}

/** Guanine */
case object G extends Nuc {
  val index = 2
  val isPyrimidine = false
}

/** Thymine */
case object T extends Nuc {
  val index = 3
  val isPyrimidine = true 
}

object DNA extends Alphabet[Nuc] {

  val elements = List(A, C, G, T)

}


