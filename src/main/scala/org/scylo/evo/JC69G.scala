package org.scylo.evo

case class JC69G( rate: Double, alpha: Double ) {

  import math._
  import org.scylo.bio.Nuc

  def substitutionProb( from: Nuc, to: Nuc, time: Double ): Double =
    if( from == to ) {
      0.25 + 0.75 * pow( 1 + (4 * rate * time ) / alpha , -alpha)
    } else {
      0.25 - 0.25 * pow( 1 + (4 * rate * time ) / alpha , -alpha)
    }

}
