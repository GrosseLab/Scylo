package org.scylo.evo

/**
 *
 * @param rateAC instantanious substitution rate from A to C (a)
 * @param rateAG instantanious substitution rate from A to G (b)
 * @param rateAT instantanious substitution rate from A to T (c)
 * @param rateCG instantanious substitution rate from C to G (d)
 * @param rateCT instantanious substitution rate from C to T (e)
 * @param pi stationary distribution of (A, C, G, T)
 */
case class GTR(rateAC: Double, rateAG: Double, rateAT: Double, rateCG: Double, rateCT: Double, piA: Double, piC: Double, piG: Double, piT: Double) {

  import math._
  import org.scylo.util.EigenSystem
  import org.scylo.util.Matrix
  import org.scylo.util.SymEigenDecomp

  def this(rateAC: Double, rateAG: Double, rateAT: Double, rateCG: Double, rateCT: Double, pi: Array[Double]) =
    this(rateAC, rateAG, rateAT, rateCG, rateCT, pi(0), pi(1), pi(2), pi(3))

  require(abs(piA + piC + piG + piT - 1) <= 1E-10)

  val rateGT = 1.0
  val diagA = -(rateAC * piC + rateAG * piG + rateAT * piT)
  val diagC = -(rateAC * piA + rateCG * piG + rateCT * piT)
  val diagG = -(rateAG * piA + rateCG * piC + rateGT * piT)
  val diagT = -(rateAT * piA + rateCT * piC + rateGT * piG)

  val scale = - (piA * diagA + piC * diagC + piG * diagG + piT * diagT)
  val piAs = sqrt( piA )
  val piCs = sqrt( piC )
  val piGs = sqrt( piG )
  val piTs = sqrt( piT )

  val B = Matrix.wrap( 3, 3, Array(
    diagA, rateAC * piCs * piAs, rateAG * piGs * piAs, rateAT * piTs * piAs,
    rateAC * piAs * piCs, diagC, rateCG * piGs * piCs, rateCT * piTs * piCs,
    rateAG * piAs * piGs, rateCG * piCs * piGs, diagC, rateGT * piTs * piGs,
    rateAT * piAs * piTs, rateCT * piCs * piTs, rateGT * piG * piTs, diagT))

  val EigenSystem(r, ri, lambda) = SymEigenDecomp( B )


}
