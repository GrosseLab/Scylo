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
      val tree = Branch( 
        10, Leaf(A),
        20, Leaf(C))

      val dist = Felsenstein( model ).likelihood( tree )

      println( dist.mkString(" ") )
    }

  }

}
