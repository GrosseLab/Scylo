package org.scylo.util

object LUDecomp extends (Matrix => LowerUpper) { 

  import math._

  private val TINY = 1E-40

  def apply( mat: Matrix ): LowerUpper = {
    require( mat.rows == mat.columns, "LUDecomposition needs a square matrix.")

    val lu = Matrix.copy( mat )
    val index = new Array[Int]( mat.rows )

    val vv = new Array[Double]( mat.rows ) // scaling
    var temp = 0.0
    var big, d = 1.0
    var k, i, j, imax = 0
    while( i < mat.rows ) {
      big = 0.0
      j = 0
      while( j < mat.rows ) {
        temp = abs( lu(i, j) )
        if( temp > big ) big = temp
        j += 1
      } // end while
      if( big == 0.0 ) throw new Exception( "Singular matrix" )
      vv(i) = 1.0 / big // save the scaling
      i += 1
    } // end while

    while( k < mat.rows ) {
      big = 0.0
      imax = k
      i = k
      while( i < mat.rows ) {
        temp = vv(i) * abs( lu(i, k) )
        if( temp > big ) {
          big = temp
          imax = i
        }
        i += 1
      } // end while

      if( k != imax ) {
        j = 0
        while( j < mat.rows ) { // change rows
          temp = lu(imax, j)
          lu(imax, j) = lu(k, j)
          lu(k, j) = temp
          j += 1
        }
        d = -d
        vv( imax ) = vv( k )
      }
      index( k ) = imax
      if( lu(k, k) == 0.0 ) lu(k, k) = TINY
      
      i = k + 1
      while( i < mat.rows ) {
        lu(i, k) /= lu(k, k)
        temp = lu(i, k)
        j = k + 1
        while( j < mat.rows ) {
          lu(i, j) -= temp * lu(k, j)
          j += 1
        }
        i += 1
      }
      k += 1
    }
    new LowerUpper( index, lu )
  }

}

