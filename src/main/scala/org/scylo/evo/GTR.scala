package org.scylo.evo

import org.ejml.data.DenseMatrix64F
import org.ejml.factory.DecompositionFactory
import org.ejml.ops.CommonOps

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

  def this(rateAC: Double, rateAG: Double, rateAT: Double, rateCG: Double, rateCT: Double, pi: Array[Double]) =
    this(rateAC, rateAG, rateAT, rateCG, rateCT, pi(0), pi(1), pi(2), pi(3))

  require(abs(piA + piC + piG + piT - 1) <= 1E-10)

  val rateGT = 1.0
  val diagA = -(rateAC * piC + rateAG * piG + rateAT * piT)
  val diagC = -(rateAC * piA + rateCG * piG + rateCT * piT)
  val diagG = -(rateAG * piA + rateCG * piC + rateGT * piT)
  val diagT = -(rateAT * piA + rateCT * piC + rateGT * piG)

  val scale = - (piA * diagA + piC * diagC + piG * diagG + piT * diagT)

  val P = new DenseMatrix64F( Array(
    Array(sqrt(piA), 0.0, 0.0, 0.0),
    Array(0.0, sqrt(piC), 0.0, 0.0),
    Array(0.0, 0.0, sqrt(piG), 0.0),
    Array(0.0, 0.0, 0.0, sqrt(piT))))

  val Pi = new DenseMatrix64F( Array(
    Array(1.0 / sqrt(piA), 0.0, 0.0, 0.0),
    Array(0.0, 1.0 / sqrt(piC), 0.0, 0.0),
    Array(0.0, 0.0, 1.0 / sqrt(piG), 0.0),
    Array(0.0, 0.0, 0.0, 1.0 / sqrt(piT))))

  val rateMat = new DenseMatrix64F( Array(
    Array(diagA, rateAC * piC, rateAG * piG, rateAT * piT),
    Array(rateAC * piA, diagC, rateCG * piG, rateCT * piT),
    Array(rateAG * piA, rateCG * piC, diagC, rateGT * piT),
    Array(rateAT * piA, rateCT * piC, rateGT * piG, diagT)))

  CommonOps.scale( scale, rateMat )
  val tmp = new DenseMatrix64F(4, 4)
  val B = new DenseMatrix64F(4,4)
  CommonOps.mult(P, rateMat, tmp)
  CommonOps.mult(tmp, Pi, B)

  val fac = DecompositionFactory.eig( 4, true, true)
  fac.decompose( rateMat )

  val R = new DenseMatrix64F(Array( // eigen vectors as rows
  fac.getEigenVector( 0 ).data,
  fac.getEigenVector( 1 ).data,
  fac.getEigenVector( 2 ).data,
  fac.getEigenVector( 3 ).data))
  CommonOps.transpose( R )
  println( fac.getEigenvalue(0) )
  println( fac.getEigenvalue(1) )
  println( fac.getEigenvalue(2) )
  println( fac.getEigenvalue(3) )

  val left = new DenseMatrix64F(4, 4)
  CommonOps.mult(Pi, R, left)

  val Ri = new DenseMatrix64F(4,4)
  CommonOps.invert(R, Ri)
  val right = new DenseMatrix64F(4, 4)
  CommonOps.mult(Ri, P, right)

  val lambda = new DenseMatrix64F( Array(
    Array(exp(fac.getEigenvalue(0).real), 0.0, 0.0, 0.0),
    Array(0.0, exp(fac.getEigenvalue(1).real), 0.0, 0.0),
    Array(0.0, 0.0, exp(fac.getEigenvalue(2).real), 0.0),
    Array(0.0, 0.0, 0.0, exp(fac.getEigenvalue(3).real))))


  val E = new DenseMatrix64F(4, 4)
  CommonOps.mult(left, lambda, tmp)
  CommonOps.mult(tmp, right, E)

}
