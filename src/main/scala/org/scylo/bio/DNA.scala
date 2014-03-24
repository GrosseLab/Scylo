package org.scylo.bio

sealed trait DNA
case object A extends DNA
case object C extends DNA
case object G extends DNA
case object T extends DNA
