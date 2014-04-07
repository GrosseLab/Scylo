package org.scylo.tree

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.scylo.bio._

class BranchSlideTest extends FunSpec with Matchers {

  describe("The Branch Slide operator") {

    it("can propose a new tree") {

      val tree = 
        Branch(
          50, Branch(
            10, Leaf(A),
            20, Leaf(C)),
          30, Leaf(T))

      val operator = BranchSlide( 0.0 )
      val newTree = operator.propose(tree, null)

      println( tree.heights() )
      println( tree.leafs )
      println( tree.print("") )
      println( newTree.print("") )
    }

  }

}
