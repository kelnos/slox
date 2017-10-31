import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.spurint",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "slox",
    libraryDependencies ++= Seq(
      scalaParserCombinators,
      scalaTest % Test,
    ),
    mainClass in assembly := Some("org.spurint.slox.Lox"),
  )
