package org.spurint.slox

import java.nio.charset.Charset
import scala.io.Source

object Lox extends App {
  private var hadError = false

  private def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println(s"[line $line] Error $where: $message")
    hadError = true
  }

  private def run(source: String): Unit = {
    Scanner(source).foreach { token =>
      println(token)
    }
  }

  private def runFile(path: String): Unit = {
    run(Source.fromFile(path, "UTF-8").mkString)
    if (hadError) {
      sys.exit(65)
    }
  }

  private def runPrompt(): Unit = {
    print("> ")
    Source.fromInputStream(System.in, Charset.defaultCharset.displayName).getLines().foreach { line =>
      Option(line).foreach(run)
      hadError = false
      print("> ")
    }
  }

  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0))
    case _ => println("Usage: slox [script]")
  }
}
