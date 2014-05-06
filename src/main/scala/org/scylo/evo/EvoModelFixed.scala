package org.scylo.evo

import org.lanyard.random.RNG

trait EvoModelFixed[A] {

  def ?> (from: A, to: A) : Double

  def substitutioProb( from: A, to: A): Double 

  def !> (from: A, source: RNG): A

  def mutate( from: A, source: RNG): A

}
