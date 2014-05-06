package org.scylo.util

class LowerUpper( private val index: Array[Int], private val lu: Matrix ) {

  def \ ( b: Array[Double] ): Array[Double] = solve( b )

  /** Solves A x = b */
  def solve( b: Array[Double] ): Array[Double] = {
    var i, ii, ip, j = 0
    var sum = 0.0

    // FIXME: Add dimension check

    val x = new Array[Double]( lu.columns )
    System.arraycopy( b, 0, x, 0, b.length )

    while( i < lu.rows ) {
      ip = index( i )
      sum = x( ip )
      x( ip ) = x( i )
      if( ii != 0 ) {
        j = ii - 1
        while( j < i ) { sum -= lu(i, j) * x(j); j += 1 }
      } else if( sum != 0.0 ) {
        ii = i + 1
      }
      x( i ) = sum
      i += 1
    }

    i = lu.rows - 1
    while( i >= 0 ) {
      sum = x(i)
      j = i + 1
      while( j < lu.rows ) { sum -= lu(i, j) * x(j); j +=1 }
      x(i) = sum / lu(i, i)
      i -= 1
    }
    x
  }


  def \ ( b: Matrix ): Matrix = solve( b )

  def solve( b: Matrix ): Matrix = ???

  def ! : Matrix = inverse

  def inverse: Matrix = ???

}
