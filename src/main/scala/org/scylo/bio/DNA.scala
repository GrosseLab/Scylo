package org.scylo.bio

sealed trait Nuc
case object A extends Nuc
case object C extends Nuc
case object G extends Nuc
case object T extends Nuc

object DNA extends Alphabet[Nuc] {

  val elements = List(A, C, G, T)

}


