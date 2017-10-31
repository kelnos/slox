package org.spurint.slox

import java.nio.charset.Charset
import scala.io.Source

object Lox extends App {
  case class LoxError(line: Int, message: String)

  private def reportError(err: LoxError): Unit = {
    System.err.println(s"ERROR:${err.line}: ${err.message}")
  }

  private def dumpAst(expr: Expr): Unit = {
    import ExprAstPrinters._
    println(AstPrinter(expr))
  }

  private def run(source: String): Either[Seq[LoxError], Expr] = {
    val tokens = Scanner(source)
    val invalidTokens = tokens.collect { case t @ Token(Token.Type.Invalid(_), _, _, _) => t }
    if (invalidTokens.nonEmpty) {
      Left(invalidTokens.map(token => LoxError(token.line, s"Invalid token: ${token.literal}")))
    } else {
      RecursiveDescentParser(tokens).swap.map { err =>
        val actual = err.actual.headOption.map(_.lexeme).getOrElse("(unknown)")
        val line = err.actual.headOption.map(_.line).getOrElse(-1)
        val expected = err.expected.map(_.lexeme).mkString(", ")
        Seq(LoxError(line, s"Unexpected token $actual; expected $expected"))
      }.swap
    }
  }

  private def runFile(path: String): Unit = {
    run(Source.fromFile(path, "UTF-8").mkString) match {
      case Left(errors) => errors.foreach(reportError); sys.exit(1)
      case Right(expr) => dumpAst(expr)
    }
  }

  private def runPrompt(): Unit = {
    print("> ")
    Source.fromInputStream(System.in, Charset.defaultCharset.displayName).getLines().foreach { line =>
      run(line).fold(_.foreach(reportError), dumpAst)
      print("> ")
    }
  }

  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0))
    case _ => println("Usage: slox [script]"); sys.exit(1)
  }
}
