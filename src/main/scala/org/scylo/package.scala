package org

import org.scylo.bio._

package object scylo {

  def isSubstitution(nuc1: Nuc, nuc2: Nuc): Boolean = nuc1 != nuc2

  def isTransition(nuc1: Nuc, nuc2: Nuc): Boolean = (nuc1.isPurine && nuc2.isPurine) || (nuc1.isPyrimidine && nuc2.isPyrimidine)

  def isTransversion(nuc1: Nuc, nuc2: Nuc): Boolean = (nuc1.isPurine && nuc2.isPyrimidine) || (nuc1.isPyrimidine && nuc2.isPurine)

  def countMismatches[A](seq1: List[A], seq2: List[A]): (Int, Int) = countWithLength(seq1, seq2)(_ != _)

  def countMatches[A](seq1: List[A], seq2: List[A]): (Int, Int) = countWithLength(seq1, seq2)(_ == _)

  def countTransitions(seq1: List[Nuc], seq2: List[Nuc]): (Int, Int) = countWithLength(seq1, seq2)(isTransition)

  def countTransversions(seq1: List[Nuc], seq2: List[Nuc]): (Int, Int) = countWithLength(seq1, seq2)(isTransversion)

  def countTrans(seq1: List[Nuc], seq2: List[Nuc]): (Int, Int, Int) = countWithLength2(seq1, seq2, isTransition, isTransversion)

  /** Counts two two types of transitions (TC, AG) and transversions. */
  def countTrans2(seq1: List[Nuc], seq2: List[Nuc]): (Int, Int, Int, Int) = {
    // the fold keeps a counter for every mutation of interest and the length
    seq1.zip(seq2).foldLeft((0, 0, 0, 0)) { (acc, elem) =>
      if( elem._1 == G && elem._2 == A || elem._1 == A && elem._2 == G ) { // type 1 transition
        (acc._1 + 1, acc._2, acc._3, acc._4 + 1)
      } else if( elem._1 == C && elem._2 == T || elem._1 == T && elem._2 == C ) { // type 2 transition
        (acc._1, acc._2 + 1, acc._3, acc._4 + 1)
      } else if( isTransversion( elem._1, elem._2 ) ) { // transversion
        (acc._1, acc._2, acc._3 + 1, acc._4 + 1)
      } else {
        (acc._1, acc._2, acc._3, acc._4 + 1)
      }
    }
  }

  def countWithLength[A](seq1: List[A], seq2: List[A])(pred: (A, A) => Boolean): (Int, Int) = seq1.zip(seq2).foldLeft((0, 0)) { (acc, elem) =>
    if (pred(elem._1, elem._2)) (acc._1 + 1, acc._2 + 1) else (acc._1, acc._2 + 1)
  }

  def countWithLength2[A](seq1: List[A], seq2: List[A], pred1: (A, A) => Boolean, pred2: (A, A) => Boolean): (Int, Int, Int) =
    seq1.zip(seq2).foldLeft((0, 0, 0)) { (acc, elem) =>
      // the case where both predicates evaluate to true is considered to be impossible as with transitions and transversions
      if (pred1(elem._1, elem._2))
        (acc._1 + 1, acc._2, acc._3 + 1)
      else if (pred2(elem._1, elem._2))
        (acc._1, acc._2 + 1, acc._3 + 1)
      else
        (acc._1, acc._2, acc._3 + 1)
    }
}
