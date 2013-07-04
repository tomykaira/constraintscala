import AssemblyKeys._

name := "ConstraintScala"

organization := "com.tomykaira"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

resolvers += "jgit-repository" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"

initialCommands := "import com.tomykaira.constraintscala._ com.tomykaira.uchronie._"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

mainClass := Some("com.tomykaira.uchronie.Main")

assemblySettings
