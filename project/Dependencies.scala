import sbt._

object Dependencies {
  private lazy val versions = Map(
    "logback" -> "1.2.3",
    "scala-parser-combinators" -> "1.0.6",
    "scalatest" -> "3.0.3",
    "slf4j" -> "1.7.25",
  )

  lazy val logback = "ch.qos.logback" % "logback-classic" % versions("logback")
  lazy val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % versions("scala-parser-combinators")
  lazy val scalaTest = "org.scalatest" %% "scalatest" % versions("scalatest")
  lazy val slf4j = "org.slf4j" % "slf4j-api" % versions("slf4j")
}
