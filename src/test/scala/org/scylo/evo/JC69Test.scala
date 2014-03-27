package org.scylo.evo

import org.scylo.bio._

import org.lanyard.random.KISS

import org.scalatest.FunSpec
import org.scalatest.Matchers

class JC69Test extends FunSpec with Matchers {

  describe("The Jukes-Cantor model") {

    it("can mutate a nucleotide.") {

      val rng = KISS(482394234552L)
      val jc = JC69(0.1)
      val tmp = List(A, C, G, T, A, A, G, C, T, A, T, A, T, A, C, T, A, T, A, G, G, G, C, G, T, A, A, C, T)
      val seq = tmp ::: tmp ::: tmp ::: tmp ::: tmp ::: tmp

      val (mutant, nextRNG) = jc.mutate(seq, 100, rng)

      val matches = seq.zip(mutant).count { pair => pair._1 == pair._2 }

      println(seq.mkString)
      println(mutant.mkString)
      println( matches + " of: " + seq.length )

    }

  }

}
