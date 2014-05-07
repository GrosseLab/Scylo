package org.scylo.util

class Matrix(val rows: Int, val columns: Int, private val values: Array[Double]) {

  require(rows > 0, "Can not initiate matrix with less than one row.")
  require(columns > 0, "Can not initiate matrix with less than one column.")
  require(values.length == rows * columns, "Number of  elements was not equal to rows * columns")

  def apply(i: Int, j: Int): Double = values(i * columns + j)

  def update(i: Int, j: Int, value: Double): Unit = values(i * columns + j) = value

  override def toString: String = {
    import scala.collection.mutable.StringBuilder
    val builder = new StringBuilder(50)
    for( i <- 0 until rows ) {
      for( j <- 0 until columns ) {
        builder ++= apply(i, j) + "\t"
      }
      builder += '\n'
    }
    builder.result
  }

}

object Matrix {

  def copy(mat: Array[Array[Double]]): Matrix = {
    val rows = mat.length
    if (rows == 0) // empty array
      new Matrix(0, 0, Array.empty[Double])
    else {
      val columns = mat(0).length
      val newValues = new Array[Double](rows * columns)
      var i = 0
      while (i < rows) { // copy each row
        System.arraycopy(mat(i), 0, newValues(i), 0, columns)
        i += 1
      }
      new Matrix(rows, columns, newValues)
    }
  }

  def copy(mat: Matrix): Matrix = {
    val newValues = new Array[Double](mat.rows * mat.columns)
    System.arraycopy(mat.values, 0, newValues, 0, mat.rows * mat.columns)
    new Matrix(mat.rows, mat.columns, newValues)
  }

  def wrap(rows: Int, columns: Int, values: Array[Double]): Matrix =
    new Matrix(rows, columns, values)

  def apply(rows: Int, columns: Int): Matrix =
    new Matrix(rows, columns, new Array[Double](rows * columns))

  def ones(rows: Int, columns: Int): Matrix =
    new Matrix(rows, columns, Array.fill(rows * columns)(1.0))

  def id( rows: Int, columns: Int ): Matrix = {
    val mat = ones( rows, columns)
    var i = 0
    while( i < rows ) {
      mat(i, i) = 1
      i += 1
    }
    mat
  }

}
