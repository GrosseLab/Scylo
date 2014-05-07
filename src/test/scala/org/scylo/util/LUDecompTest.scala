package org.scylo.util

import org.scalatest.FunSpec
import org.scalatest.Matchers

class LUDecompTest extends FunSpec with Matchers {

  describe("The LU decomposition") {

    it("can solve a system of linear equations (Example 1)") {
      val mat = Matrix.wrap(2, 2, Array(1.0, 2.0, 3.0, 4.0))
      val b = Array(1.0, 2.0)

      val x = LUDecomp(mat) \ b
      x(0) should equal ( 0.0 +- 1E-15 )
      x(1) should equal ( 0.5 +- 1E-15 )
    }

    it("can solve a system of linear equations (Example 2)") {
      val mat = Matrix.wrap(4, 4, Array(0, 2, 0, 1, 2, 2, 3, 2, 4, -3, 0, 1, 6, 1, -6, -5).map( _.toDouble) )
      val b = Array(1.0, 2.0, 2.0, 4.0)

      val lu = LUDecomp(mat) 
      val x = lu \ b

      x(0) should equal ( 0.6666666666666 +- 1E-7 )
      x(1) should equal ( 0.3333333333333 +- 1E-7 )
      x(2) should equal ( -0.222222222222 +- 1E-7 )
      x(3) should equal ( 0.3333333333333 +- 1E-7 )
    }
  }

}
