package org.scylo.tree

import org.lanyard.random.KISS
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scylo.bio._

class TreeTest extends FunSpec with Matchers {

  describe("A tree") {

    it("has a string representation") {
      val tree = 
        Branch(
          50, Branch(
            10, Leaf(A),
            20, Leaf(C)),
          30, Leaf(T))
    }

    it("can be be flipped") {

      val tree = 
        Branch(
          50, Branch(
            10, Leaf(A),
            20, Leaf(C)),
          30, Leaf(T))

      val rng = KISS( 423895 )
      val (draw1, rng1) = rng.nextBoolean
      val (draw2, rng2) = rng1.nextBoolean

      println( draw1, draw2 )

      val (flippedTree, _) = tree.flip( rng  )

      println( tree.print("") )
      println( flippedTree.print("") )
    }

  }

}
