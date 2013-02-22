name := "applied-nlp"

version := "0.2.0"

organization := "edu.utexas"

scalaVersion := "2.10.0"

retrieveManaged := true

crossPaths := false

resolvers ++= Seq(
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "0.8.0",
  "gov.nist.math" % "jama" % "1.0.2"
)
