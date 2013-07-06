import AssemblyKeys._

name := "ConstraintScala"

organization := "com.tomykaira"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

resolvers += "jgit-repository" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.4"

initialCommands := "import com.tomykaira.constraintscala._; import com.tomykaira.uchronie._"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

mainClass := Some("com.tomykaira.uchronie.Main")

assemblySettings
