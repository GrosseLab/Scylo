package org.scylo.algo

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.scylo.evo._
import org.scylo.tree._
import org.scylo.bio._

class FelsensteinTest extends FunSpec with Matchers {

  describe("The Felsenstein pruning algorithm") {

    it("can comput the likelihood") {
      val model = JC69( 0.0005 )


      var sum = 0.0
      for( nuc1 <- DNA.elements; nuc2 <- DNA.elements ) {
        val tree = Branch(
          10, Leaf(nuc1),
          20, Leaf(nuc2))

        val like = Felsenstein( model ).likelihood( tree )
        sum += like
      }
      println( sum )
    }

  }

}
