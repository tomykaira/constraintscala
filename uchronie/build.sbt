import AssemblyKeys._

name := "Uchronie"

organization := "com.tomykaira"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.0"

resolvers += "jgit-repository" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.0.0.201306101825-r"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.1"

initialCommands := "import com.tomykaira.uchronie._"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

fork in run := true

mainClass := Some("com.tomykaira.uchronie.Main")

assemblySettings
