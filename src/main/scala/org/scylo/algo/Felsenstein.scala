package org.scylo.algo

import org.scylo.bio._
import org.scylo.evo.JC69
import org.scylo.tree._

case class Felsenstein( model: JC69 ) {

  def DNA2Int(nuc: Nuc): Int = nuc match {
    case A => 0
    case C => 1
    case G => 2
    case T => 3
  }

  def likelihood(tree: Tree[Nuc]): Array[Double] = tree match {
    case Branch(timeL, left, timeR, right) =>
      val distL = likelihood(left)
      val distR = likelihood(right)
      val dist = new Array[Double](4)
      for (old <- List(A, C, G, T)) {
        val i = DNA2Int(old)
        for (nuc1 <- List(A, C, G, T); nuc2 <- List(A, C, G, T)) {
          dist(i) += model.substitutionProb(old, nuc1, timeL) * distL(DNA2Int(nuc1)) * model.substitutionProb(old, nuc2, timeR) * distR(DNA2Int(nuc2))
        }
      }
      dist
    case Leaf(nuc1) => nuc1 match {
      case A => Array(1.0, 0.0, 0.0, 0.0)
      case C => Array(0.0, 1.0, 0.0, 0.0)
      case G => Array(0.0, 0.0, 1.0, 0.0)
      case T => Array(0.0, 0.0, 0.0, 1.0)
    }
  }

}

