import sbt._
import Keys._

object HelloBuild extends Build {
  lazy val root = Project(id = "csproject", base = file(".")) aggregate(cs, uchronie)

  lazy val cs = Project(id = "constraintscala", base = file("constraintscala"))

  lazy val uchronie = Project(id = "uchronie", base = file("uchronie")) dependsOn(cs)
}
