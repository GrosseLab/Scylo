package org.scylo.util

case class EigenSystem private[util] ( 
  val eigenVec: Matrix,
  val invEigenVec: Matrix,
  val eigenVal: Array[Double] )
