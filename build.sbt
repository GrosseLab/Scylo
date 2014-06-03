name := "Scylo"

description := "A library for phylogenetic simulations in scala."

version := "0.0.1"

scalaVersion := "2.11.0"

organization := "org.scylo"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
  "Lanyard Repo" at "http://s243665865.online.de/repos/lanyard"
)

libraryDependencies ++= Seq (
//  "org.scalanlp" % "breeze-natives_2.10" % "0.7",
//  "gov.nist.math" % "jama" % "1.0.3",
  "com.googlecode.efficient-java-matrix-library" % "ejml" % "0.24",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.lanyard" %% "lanyard" % "0.0.1"
)

scalacOptions ++= Seq ("-unchecked", "-feature", "-deprecation", "-optimize" )

crossScalaVersions := Seq("2.10.4", "2.11.0")

publishMavenStyle := true

publishTo := Some(Resolver.sftp("My Private Repo", "ftp.mybioinformatics.org", "repos/scylo"))
