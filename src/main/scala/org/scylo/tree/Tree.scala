package org.scylo.tree

import org.lanyard.dist.cont.Gamma
import org.lanyard.random.RNG
import org.lanyard.random.Random

/** Phylogenetic tree */
sealed trait Tree[+A]
case class Branch[+A]( timeLeft: Double, left: Tree[A], timeRight: Double, right: Tree[A] ) extends Tree[A]
case class Leaf[+A]( value: A ) extends Tree[A]

object Tree {

  def uniform[A]( nrOfLeafs: Int, timeDist: Gamma ): Random[Tree[A]] = new Random[Tree[A]] {

    def random( source: RNG ): (Tree[A], RNG) = ???

  }

}
