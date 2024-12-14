val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AdventOfScala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0",
    libraryDependencies += "org.scalanlp" %% "breeze-viz" % "2.1.0"
  )
