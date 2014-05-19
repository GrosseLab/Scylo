package org.scylo.evo

import org.lanyard.dist.Distribution
import org.lanyard.random.RNG

trait EvoModelFixed[A] extends Distribution[(A, A)] {

  def ?(from: A, to: A): Double = substitutionProb(from, to)

  def substitutionProb(from: A, to: A): Double

  def !(from: A, source: RNG): (A, RNG) = random(from, source)

  def mutate(from: A, source: RNG): (A, RNG) = random(from, source)

  def random(from: A, source: RNG): (A, RNG)

}
