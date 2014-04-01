package org.scylo.tree

import org.lanyard.dist.cont.Gamma
import org.lanyard.random.RNG
import org.lanyard.random.Random

/** Phylogenetic tree */
sealed trait Tree[+A] {

  def flip( source: RNG ): (Tree[A], RNG)

  def print( prefix: String ): String

  def isLeaf: Boolean

  def nrOfNodes: Int

  def nrOfLeafs: Int

}

case class Branch[+A]( timeLeft: Double, left: Tree[A], timeRight: Double, right: Tree[A] ) extends Tree[A] {

  def flip( source: RNG ): (Tree[A], RNG) = {
    val (flip, rng0) = source.nextBoolean
    val (newRight, rng1) = right.flip( rng0 )
    val (newLeft, rng2) = left.flip( rng1 )
    if( flip ) {
      (Branch( timeRight, newRight, timeLeft, newLeft ), rng2 )
    } else {
      (Branch( timeLeft, newLeft, timeRight, newRight ), rng2 )
    }
  }

  def isLeaf: Boolean = false

  def print( prefix: String): String = this match {
    case Branch( timeLeft, Leaf(l), timeRight, Leaf(r) ) => // both leafs
      s"$prefix├── $timeRight : $r\n" +
      s"$prefix└── $timeLeft : $l"
    case Branch( timeLeft, Leaf(l), timeRight, right ) => // left leaf
      s"$prefix├── $timeRight\n" + 
      right.print( prefix + "│   " ) + "\n" +
      s"$prefix└── $timeLeft : $l" 
    case Branch( timeLeft, left, timeRight, Leaf(r) ) => // right leaf
      s"$prefix├── $timeRight : $r\n" +
      s"$prefix└── $timeLeft\n" +
      left.print( prefix + "    " )
    case Branch( timeLeft, left, timeRight, right ) => // no leafs
      s"$prefix├── $timeRight\n" +
      right.print( prefix + "│   " ) + "\n" +
      s"$prefix└── timeLeft\n" +
      left.print( prefix + "    " )
  }

  def nrOfNodes: Int = 1 + left.nrOfNodes + right.nrOfNodes

  def nrOfLeafs: Int = left.nrOfLeafs + right.nrOfLeafs

}

case class Leaf[+A]( value: A ) extends Tree[A] {

  def flip( source: RNG ): (Tree[A], RNG) = (this, source)

  def isLeaf: Boolean = true

  def print( prefix: String): String = prefix + "└── " + value 

  def nrOfNodes: Int = 1

  def nrOfLeafs: Int = 1

}

