package org.scylo.tree

import org.lanyard.random.RNG

trait TreeOperator {

  def propose[A]( tree: Tree[A], source: RNG ): Tree[A]

}
