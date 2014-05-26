import AssemblyKeys._

name := "Uchronie"

organization := "io.github.tomykaira"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.1"

resolvers += "jgit-repository" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.7" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

initialCommands := "import io.github.tomykaira.uchronie._"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

fork in run := true

mainClass := Some("io.github.tomykaira.uchronie.Main")

assemblySettings
