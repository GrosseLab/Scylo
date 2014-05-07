package org.scylo.util

case class EigenSystem private[util] ( 
  private val eigenVec: Matrix,
  private val invEigenVec: Matrix,
  private val eigenVal: Array[Double] )
