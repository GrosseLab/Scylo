package org.scylo.util

case object EigenDecomp extends (Matrix => EigenSystem ) {

  import math._

  def apply( mat: Matrix ): EigenSystem = ???

}
