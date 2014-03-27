package org.scylo.evo

/** The Felsenstein model.
  * 
  * @param statDist stationary distribution of (A,C,G,T)
  * @param substitutionRate substitution rate
  */
class F81 private ( private val statDist: Array[Double], val substitutionRate: Double ) {

  import math._

  require( abs(statDist.sum -1 ) <= 1E-10, "F81 parameter statDist has to sum to 1. Found sum: " + statDist.sum )

}

object F81 {

  def apply( statDist: Seq[Double], substitutionRate: Double ): F81 = new F81( statDist.toArray, substitutionRate )

}
