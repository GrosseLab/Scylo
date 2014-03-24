package org.scylo.tree

/** Phylogenetic tree */
sealed trait Tree[+A]
case class Branch[+A]( timeLeft: Double, left: Tree[A], timeRight: Double, right: Tree[A] ) extends Tree[A]
case class Leaf[+A]( value: A ) extends Tree[A]
