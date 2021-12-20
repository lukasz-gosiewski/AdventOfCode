ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode",
    idePackagePrefix := Some("com.gosiewski")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"
