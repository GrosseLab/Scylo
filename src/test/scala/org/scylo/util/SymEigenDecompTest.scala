package org.scylo.util

import org.scalatest.FunSpec
import org.scalatest.Matchers

class SymEigenDecompTest extends FunSpec with Matchers {

  describe("The Eigen Decomposition of symmetric matrices.") {

    it("should work") {

      val mat = new Matrix(3, 3, Array(1, 2, 3, 2, 4, 5, 3, 5, 1).map( _.toDouble ))

      val EigenSystem(vec, ivec, values) = SymEigenDecomp(mat)

      // check for A * x = lambda x
    }
  }
}

