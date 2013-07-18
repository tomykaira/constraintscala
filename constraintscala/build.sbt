import AssemblyKeys._

name := "ConstraintScala"

organization := "io.github.tomykaira"

version := "0.2.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.2"

initialCommands := "import io.github.constraintscala._"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

fork in run := true

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("BSD-3-Clause" -> url("http://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/tomykaira/constraintscala"))

pomExtra := (
  <scm>
    <url>git@github.com:tomykaira/constraintscala.git</url>
    <connection>scm:git:git@github.com:tomykaira/constraintscala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>tomykaira</id>
      <name>tomykaira</name>
      <url>http://tomykaira.hatenablog.com</url>
    </developer>
  </developers>)
