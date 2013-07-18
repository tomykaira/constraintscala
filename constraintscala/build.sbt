import AssemblyKeys._

name := "ConstraintScala"

organization := "io.github.tomykaira"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

initialCommands := "import io.github.constraintscala._"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

fork in run := true
