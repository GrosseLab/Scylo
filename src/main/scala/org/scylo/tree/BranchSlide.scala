package org.scylo.tree

import org.lanyard.random.RNG

case class BranchSlide(delta: Double) extends TreeOperator {

  def propose[A](tree: Tree[A], source: RNG): Tree[A] = {
    val heights = tree.heights()
    val leafs = tree.leafs

    /** Returns the minimal value together with its index. */
    def minWithIndex(ls: List[Double]): (Double, Int) = {
      val min = ls.min
      val index = ls.indexOf( min )
      (min, index )
    }

    def reconstruct(leafs: List[A], heights: List[Double], currentMin: Double): Tree[A] =
      if (leafs.tail == Nil) { // recursion stop
        Leaf(leafs.head)
      } else {
        val (min, indexOfMin) = minWithIndex(heights)
        println( s"$min at $indexOfMin" )
        val (leftHeights, rightHeights) = heights.splitAt(indexOfMin) // min goes to right
        val (leftLeafs, rightLeafs) = leafs.splitAt((leftHeights.size + 1) / 2)
        Branch(
          leftHeights.min - currentMin,
          reconstruct(leftLeafs, leftHeights, leftHeights.min - currentMin),
          rightHeights.tail.min - currentMin,
          reconstruct(rightLeafs, rightHeights.tail, rightHeights.tail.min - currentMin))
      }
    reconstruct( leafs, heights, 0.0 )
  }
}
