package org.scylo.util

class LowerUpper(private val index: Array[Int], private val lu: Matrix) {

  def \(b: Array[Double]): Array[Double] = solve(b)

  /** Solves A x = b */
  def solve(b: Array[Double]): Array[Double] = {
    require(b.length == lu.rows, "Dimension of b didn't match to LU decomposition.")

    var i, ii, ip, j = 0
    var sum = 0.0
    val x = new Array[Double](b.length)
    System.arraycopy(b, 0, x, 0, b.length)

    while (i < lu.rows) {
      ip = index(i)
      sum = x(ip)
      x(ip) = x(i)
      if (ii != 0) {
        j = ii - 1
        while (j < i) { sum -= lu(i, j) * x(j); j += 1 }
      } else if (sum != 0.0) {
        ii = i + 1
      }
      x(i) = sum
      i += 1
    }

    i = lu.rows - 1
    while (i >= 0) {
      sum = x(i)
      j = i + 1
      while (j < lu.rows) { sum -= lu(i, j) * x(j); j += 1 }
      x(i) = sum / lu(i, i)
      i -= 1
    }
    x
  }

  def \(b: Matrix): Matrix = solve(b)

  def solve(b: Matrix): Matrix = {
    require(b.rows == lu.rows, "Rows of B didn't match to LU decomposition.")
    require(b.columns == lu.columns, "Columns of B didn't match to LU decomposition.")

    val mat = Matrix(b.rows, b.columns)
    val tmp = new Array[Double](b.rows)
    var i, j = 0
    while (j < b.columns) {
      i = 0
      while (i < b.rows) { tmp(i) = b(i, j); i += 1 } // copy row
      val res = solve(tmp)
      i = 0
      while (i < b.rows) { mat(i, j) = res(i); i += 1 } // copy to result
      j += 1
    }
    mat
  }

  def ! : Matrix = inverse

  def inverse: Matrix = solve(Matrix.id(lu.rows, lu.rows))

  override def toString: String = lu.toString

}
