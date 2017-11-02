import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.spurint",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "slox",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "utf-8",
      "-feature",
      "-unchecked",
      "-Xlint:infer-any",
      "-Xlint:missing-interpolator",
      "-Xlint:nullary-unit",
      "-Xlint:private-shadow",
      "-Xlint:type-parameter-shadow",
      "-Xlint:unsound-match",
      "-Ywarn-dead-code",
      "-Ywarn-extra-implicit",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-unit",
      "-Ywarn-unused:imports",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:privates",
    ),
    libraryDependencies ++= Seq(
      scalaParserCombinators,
      scalaTest % Test,
    ),
    mainClass in assembly := Some("org.spurint.slox.Lox"),
  )
