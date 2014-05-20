package org.scylo

import bio._

import org.scalatest.FunSpec
import org.scalatest.Matchers

class PackageTest extends FunSpec with Matchers {

  describe("A transversion") {
    it("is a change from purine to pyrimidine or vice versa.") {
      isTransversion(A, C) should be(true)
      isTransversion(C, A) should be(true)
      isTransversion(A, T) should be(true)
      isTransversion(T, A) should be(true)
      isTransversion(G, C) should be(true)
      isTransversion(C, G) should be(true)
      isTransversion(G, T) should be(true)
      isTransversion(T, G) should be(true)
    }
  }

  describe("A transition") {
    it("is a change from a purine to a purine or from a pyrimidine to pyrimidine.") {
      isTransition(A, G) should be(true)
      isTransition(G, A) should be(true)
      isTransition(C, T) should be(true)
      isTransition(T, C) should be(true)
    }
  }
}
