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
  import org.ejml.data.DenseMatrix64F

  def this(rateAC: Double, rateAG: Double, rateAT: Double, rateCG: Double, rateCT: Double, pi: Array[Double]) =
    this(rateAC, rateAG, rateAT, rateCG, rateCT, pi(0), pi(1), pi(2), pi(3))

  require(abs(piA + piC + piG + piT - 1) <= 1E-10)

  val (left, right, lambda) = {
    import org.ejml.ops.CommonOps
    import org.ejml.ops.EigenOps
    import org.ejml.factory.DecompositionFactory

    val rateGT = 1.0
    val diagA = -(rateAC * piC + rateAG * piG + rateAT * piT)
    val diagC = -(rateAC * piA + rateCG * piG + rateCT * piT)
    val diagG = -(rateAG * piA + rateCG * piC + rateGT * piT)
    val diagT = -(rateAT * piA + rateCT * piC + rateGT * piG)

    val scale = -(piA * diagA + piC * diagC + piG * diagG + piT * diagT)
    val piAs = sqrt(piA)
    val piCs = sqrt(piC)
    val piGs = sqrt(piG)
    val piTs = sqrt(piT)

    val scaledQ = new DenseMatrix64F(Array(
      Array(diagA / scale, rateAC * piC / scale, rateAG * piG / scale, rateAT * piT / scale),
      Array(rateAC * piA / scale, diagC / scale, rateCG * piG / scale, rateCT * piT / scale),
      Array(rateAG * piA / scale, rateCG * piC / scale, diagC / scale, rateGT * piT / scale),
      Array(rateAT * piA / scale, rateCT * piC / scale, rateGT * piG / scale, diagT /scale )))

    leftMult(scaledQ, Array(piAs, piCs, piGs, piTs))
    rightMult(scaledQ, Array(1.0 / piAs, 1.0 / piCs, 1.0 / piGs, 1.0 / piTs))

    println( scaledQ.print )

    val comp = DecompositionFactory.eig(4, true, true) // need eigen vectors, matrix is symmetric
    comp.decompose( scaledQ )

    val r = EigenOps.createMatrixV( comp )
    val ri = new DenseMatrix64F(4, 4)
    CommonOps.invert(r, ri)

    leftMult(r, Array(1.0 / piAs, 1.0 / piCs, 1.0 / piGs, 1.0 / piTs))
    rightMult(ri, Array(piAs, piCs, piGs, piTs))

    (r, ri, EigenOps.createMatrixD( comp ) )
  }

  import org.scylo.bio.Nuc
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

  private def rowSum(mat: DenseMatrix64F): Array[Double] = {
    val sums = new Array[Double](mat.numCols)
    var i, j = 0
    while (i < mat.numCols) {
      j = 0
      while (j < mat.numRows) {
        sums(i) += mat.get(i, j)
        j += 1
      }
      i += 1
    }
    sums
  }

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
