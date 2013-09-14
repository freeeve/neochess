name := "neochess"
 
version := "0.1"
 
scalaVersion := "2.9.2"

resolvers += "anormcypher" at "http://repo.anormcypher.org/"

resolvers += "codahale" at "http://repo.codahale.com/"
 
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "org.anormcypher" %% "anormcypher" % "0.3.0"
)
