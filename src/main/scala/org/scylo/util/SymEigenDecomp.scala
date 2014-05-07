package org.scylo.util

/** Eigen decomposition of symmetric matrices. */
case object SymEigenDecomp extends (Matrix => EigenSystem) {

  import math._

  private val EPSILON = 2.3E-16

  def apply(mat: Matrix): EigenSystem = {

    null
  }

  private def toTriDiag(mat: Matrix): (Matrix, Array[Double], Array[Double]) = {
    var g = 0.0
    var l, k, j, i = 0
    val z = Matrix.copy(mat)
    val d = new Array[Double](mat.rows)
    val e = new Array[Double](mat.rows)

    i = mat.rows - 1
    while (i > 0) {
      var scale, hh, h, f = 0.0
      l = i - 1
      if (l > 0) {
        k = 0
        while (k < i) { scale += abs(mat(i, k)); k += 1 }
        if (scale == 0.0) {
          e(i) = z(i, l)
        } else { // end if
          k = 0
          while (k < i) {
            z(i, k) /= scale
            h += z(i, k) * z(i, k)
            i += 1
          } // end while
          f = z(i, i)
          g = if (f >= 0.0) -sqrt(h) else sqrt(h)
          e(i) = scale * g
          h -= f * g
          z(i, l) = f - g
          f = 0.0
          j = 0
          while (j < i) {
            z(j, i) = z(i, j) / h
            g = 0.0
            k = 0
            while (k < j + 1) { g += z(j, k) * z(i, k); k += 1 }
            k = j + 1
            while (k < i) { g += z(k, j) * z(i, k); k += 1 }
            e(j) = g / h
            f += e(j) * z(i, j)
            j += 1
          } // end while
          hh = f / (h + h)
          j = 0
          while (j < i) {
            f = z(i, j)
            g = e(j) - hh * f
            e(j) = g
            k = 0
            while (k < j + 1) { z(j, k) -= f * e(k) + g * z(i, k); k += 1 }
            j += 1
          }
        } // end else
      } else { e(i) = z(i, l) } // end if
      d(i) = h
      i -= 1
    } // end else

    e(0) = 0.0
    d(0) = 0.0
    i = 0
    while (i < mat.rows) {
      if (d(i) != 0.0) {
        j = 0
        while (j < i) {
          g = 0.0
          k = 0
          while (k < i) { g += z(i, k) * z(k, j); k += 1 }
          k = 0
          while (k < i) { z(k, j) -= g * z(k, i); k += 1 }
          j += 1
        } // end while
      } // end if
      d(i) = z(i, i)
      z(i, i) = 1.0
      j = 0
      while (j < i) {
        z(i, j) = 0.0
        z(j, i) = 0.0
        j += 1
      }
      i += 1
    } // end while

    (z, d, e)
  }

  private def sign(a: Double, b: Double): Double =
    if (b >= 0) {
      if (a >= 0) a else -a
    } else {
      if (a >= 0) -a else a
    }

  private def pythag(a: Double, b: Double): Double = {
    val absa = abs(a)
    val absb = abs(b)
    if (absa > absb)
      absa * sqrt(1.0 + (absb / absa) * (absb / absa))
    else {
      if (absb == 0.0) 0.0 else absb * sqrt(1.0 + (absa / absb) * (absa / absb))
    }
  }

  private def eigOfTriDiag(mat: Matrix, d: Array[Double], e: Array[Double]): (Array[Double], Matrix) = {
    var m, l, iter, i, k = 0
    var s, r, p, g, f, dd, c, b = 0.0
    var break = true
    val n = e.length

    i = 1
    while (i < n) { e(i - 1) = e(i); i += 1 }
    e(e.length - 1) = 0.0

    while (l < n) {
      break = true
      iter = 0
      do {
        m = l
        while (m < n - 1 && break) {
          dd = abs(d(m)) + abs(d(m + 1))
          if (abs(e(m)) <= EPSILON * dd) { break = false }
          m += 1
        } // end while

        if (m != l) {
          iter += 1
          if (iter == 30) throw new Exception("bums")
          g = (d(l + 1) - d(l)) / (2.0 * e(l))
          r = pythag(g, 1.0)
          g = d(m) - d(l) + e(l) / (g + sign(r, g))
          s = 1.0
          c = 1.0
          p = 0.0

          i = m - 1
          break = true
          while (i >= l && break) {
            f = s * e(i)
            b = c * e(i)
            r = pythag(f, g)
            e(i + 1) = r
            if (r == 0.0) {
              d(i + 1) -= p
              e(m) = 0.0
              break = false
            }
            if (break) {
              s = f / r
              c = g / r
              g = d(i + 1) - p
              r = (d(i) - g) * s + 2.0 * c * b
              p = s * r
              d(i + 1) = g + p
              g = c * r - b

              k = 0
              while (k < n) {
                f = mat(k, i+1)
                mat(k, i+1) = s * mat(k, i) + c * f
                mat(k, i) = c * mat(k, i) - s * f
                k += 1
              } // end while
            } // end if
          } // end while

          if( !(r == 0.0 && i >= l ) ) {
            d(l) -= p
            e(l) = g
            e(m) = 0.0
          }
          i -= 1
        } // end if
      } while (m != l) // end do-while
      l += 1
    } // end while
    (d, mat)
  }

  private def sort(d: Array[Double], v: Matrix): EigenSystem = {
    var i, j = 0
    while (i < d.length - 1) {
      var k = i
      var p = d(k)
      j = i
      while (j < d.length) {
        if (d(j) >= p) {
          k = j
          p = d(k)
        }
        j += 1
      } // end while
      if (k != i) {
        d(k) = d(i)
        d(i) = p
        j = 0
        while (j < d.length) {
          p = v(j, i)
          v(j, i) = v(j, k)
          v(j, k) = p
          j += 1
        }
      }
      i += 1
    }
    EigenSystem(v, v, null)
  }
}
