package org.scylo.evo

import org.scylo.bio.{Nuc,DNA}

/**
 *
 * @param rateAC instantanious substitution rate from A to C (a)
 * @param rateAG instantanious substitution rate from A to G (b)
 * @param rateAT instantanious substitution rate from A to T (c)
 * @param rateCG instantanious substitution rate from C to G (d)
 * @param rateCT instantanious substitution rate from C to T (e)
 * @param pi stationary distribution of (A, C, G, T)
 */
case class GTR(rateAC: Double, rateAG: Double, rateAT: Double, rateCG: Double, rateCT: Double, statA: Double, statC: Double, statG: Double, statT: Double) 
    extends EvoModel[Nuc] {

  import math._
  import org.ejml.data.DenseMatrix64F

  def this(rateAC: Double, rateAG: Double, rateAT: Double, rateCG: Double, rateCT: Double, pi: Array[Double]) =
    this(rateAC, rateAG, rateAT, rateCG, rateCT, pi(0), pi(1), pi(2), pi(3))

  require(abs(statA + statC + statG + statT - 1) <= 1E-10)

  private val stat = Array(statA, statC,statG, statT)

  private val (left, right, lambda) = {
    import org.ejml.ops.CommonOps
    import org.ejml.ops.EigenOps
    import org.ejml.factory.DecompositionFactory

    val rateGT = 1.0
    val diagA = -(rateAC * statC + rateAG * statG + rateAT * statT)
    val diagC = -(rateAC * statA + rateCG * statG + rateCT * statT)
    val diagG = -(rateAG * statA + rateCG * statC + rateGT * statT)
    val diagT = -(rateAT * statA + rateCT * statC + rateGT * statG)

    val scale = -(statA * diagA + statC * diagC + statG * diagG + statT * diagT)
    val piAs = sqrt(statA)
    val piCs = sqrt(statC)
    val piGs = sqrt(statG)
    val piTs = sqrt(statT)

    val scaledQ = new DenseMatrix64F(Array(
      Array(diagA / scale, rateAC * statC / scale, rateAG * statG / scale, rateAT * statT / scale),
      Array(rateAC * statA / scale, diagC / scale, rateCG * statG / scale, rateCT * statT / scale),
      Array(rateAG * statA / scale, rateCG * statC / scale, diagG / scale, rateGT * statT / scale),
      Array(rateAT * statA / scale, rateCT * statC / scale, rateGT * statG / scale, diagT /scale )))

    leftMult(scaledQ, Array(piAs, piCs, piGs, piTs))
    rightMult(scaledQ, Array(1.0 / piAs, 1.0 / piCs, 1.0 / piGs, 1.0 / piTs))

    val comp = DecompositionFactory.eig(4, true, true) // need eigen vectors, matrix is symmetric
    comp.decompose( scaledQ )

    val r = EigenOps.createMatrixV( comp )
    val ri = new DenseMatrix64F(4, 4)
    CommonOps.invert(r, ri)

    leftMult(r, Array(1.0 / piAs, 1.0 / piCs, 1.0 / piGs, 1.0 / piTs))
    rightMult(ri, Array(piAs, piCs, piGs, piTs))

    (r, ri, EigenOps.createMatrixD( comp ) )
  }

  def alphabet = DNA

  def statDist( of: Nuc ): Double = stat( of.index )

  def substitutionProb(from: Nuc, to: Nuc, time: Double): Double = {
    val i = from.index
    val j = to.index
    var result = 0.0
    var k = 0
    while (k < 4) {
      result += left.get(i, k) * exp(lambda.get(k, k) * time) * right.get(k, j)
      k += 1
    }
    result
  }

  def >> (time: Double) = null

  /** in-place multiplication. Columnwise */
  private def leftMult(mat: DenseMatrix64F, diag: Array[Double]): Unit = {
    var i, j = 0
    while (i < mat.numCols) {
      j = 0
      while (j < mat.numRows) {
        mat.set(i, j, mat.get(i, j) * diag(i))
        j += 1
      }
      i += 1
    }
  }
  /** in-place multiplication. Rowwise */
  private def rightMult(mat: DenseMatrix64F, diag: Array[Double]): Unit = {
    var i, j = 0
    while (i < mat.numRows) {
      j = 0
      while (j < mat.numCols) {
        mat.set(i, j, mat.get(i, j) * diag(j))
        j += 1
      }
      i += 1
    }
  }

}
