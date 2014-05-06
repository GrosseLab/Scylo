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

      val mat = Matrix.wrap(2, 3, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0) )
      val b = Array(0.0, 4.0)

      val x = LUDecomp(mat) \ b

      println( x(0) )
      println( x(1) )
      println( x(2) )


      // x(0) should equal ( 0.064748 +- 1E-7 )
      // x(1) should equal ( 0.129496 +- 1E-7 )
      // x(2) should equal ( 0.194245 +- 1E-7 )
    }
  }

}
