package org.scylo.util

case class EigenSystem private[util] ( 
  private val eigenVec: Array[Array[Double]],
  private val invEigenVec: Array[Array[Double]],
  private val eigenVal: Array[Double] )
