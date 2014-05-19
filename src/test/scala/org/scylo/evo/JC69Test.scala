package org.scylo.evo

import org.scalacheck.Gen

import org.scylo.bio._

import org.lanyard.random.KISS

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class JC69Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import JC69Test._

  describe("The Jukes-Cantor model") {

    it("is a proper probability distribution") {
      forAll( 
        (JC69gen, "Jukes-Cantor model"),
        (Gen.choose( 0.0, 20.0), "time")
      ) { (jc: JC69, time: Double) =>
        for( from <- List(A, C, G, T) ) {
          val probs = List(A, C, G, T).map( jc.withTime( time ) ? (from, _ )).sum
          probs should be( 1.0 +- 1E-15)
        }
      }

    }

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

object JC69Test {

  val maxRate = 10.0
  val maxTime = 20.0

  val JC69gen = for {
    rate <- Gen.choose(0.0, maxRate)
  } yield JC69( rate )
}
