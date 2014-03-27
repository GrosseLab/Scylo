package org

import org.scylo.bio._

package object scylo {

  def isSubstitution(nuc1: Nuc, nuc2: Nuc): Boolean = nuc1 != nuc2

  def isTransition(nuc1: Nuc, nuc2: Nuc): Boolean = (nuc1.isPurine && nuc2.isPurine) || (nuc1.isPyrimidine && nuc2.isPyrimidine)

  def isTransversion(nuc1: Nuc, nuc2: Nuc): Boolean = (nuc1.isPurine && nuc2.isPyrimidine) || (nuc1.isPyrimidine && nuc2.isPurine)

  def countMismatches[A](seq1: List[A], seq2: List[A]): (Int, Int) = countWithLength(seq1, seq2)( _ != _ )

  def countMatches[A](seq1: List[A], seq2: List[A]): (Int, Int) = countWithLength(seq1, seq2)( _ == _ )

  def countTransitions(seq1: List[Nuc], seq2: List[Nuc]): (Int, Int) = countWithLength(seq1, seq2)( isTransition)

  def countTransversions(seq1: List[Nuc], seq2: List[Nuc]): (Int, Int) = countWithLength(seq1, seq2)( isTransversion)

  def countTrans( seq1: List[Nuc], seq2: List[Nuc] ): (Int, Int, Int) = countWithLength2(seq1, seq2, isTransition, isTransversion )

  def countWithLength[A](seq1: List[A], seq2: List[A])( pred: (A, A) => Boolean ): (Int, Int) =  seq1.zip(seq2).foldLeft((0, 0)) { (acc, elem) =>
    if (pred(elem._1, elem._2) ) (acc._1 + 1, acc._2 + 1) else (acc._1, acc._2 + 1) }

  def countWithLength2[A](seq1: List[A], seq2: List[A], pred1: (A, A) => Boolean, pred2: (A, A) => Boolean ): (Int, Int, Int) =  seq1.zip(seq2).foldLeft((0, 0, 0)) { (acc, elem) =>
    if (pred1(elem._1, elem._2) && pred2(elem._1, elem._2) ) 
      (acc._1 + 1, acc._2 + 1, acc._3 + 1) 
    else if( pred1(elem._1, elem._2) )
      (acc._1 + 1, acc._2, acc._3 + 1)
    else if( pred2(elem._1, elem._2) )
      (acc._1, acc._2 + 1, acc._3 + 1)
    else
      (acc._1, acc._2, acc._3 + 1)
  }
}
