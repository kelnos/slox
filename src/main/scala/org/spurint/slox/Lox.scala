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

  private def scan(source: String): Either[Seq[LoxError], Seq[Token]] = {
    val tokens = Scanner(source)
    val invalidTokens = tokens.collect { case t @ Token(Token.Type.Invalid(_), _, _, _) => t }
    if (invalidTokens.nonEmpty) {
      Left(invalidTokens.map(token => LoxError(token.line, s"Invalid token: ${token.literal}")))
    } else {
      Right(tokens)
    }
  }

  private def parse(tokens: Seq[Token]): Either[Seq[LoxError], Seq[Stmt]] = {
    RecursiveDescentParser(tokens).swap.map { err =>
      val actual = err.actual.headOption.map(_.lexeme).getOrElse("(unknown)")
      val line = err.actual.headOption.map(_.line).getOrElse(-1)
      val expected = err.expected.map(_.lexeme).mkString(", ")
      Seq(LoxError(line, s"Unexpected token $actual; expected $expected"))
    }.swap
  }

  private def interpret(stmts: Seq[Stmt]): Either[Seq[LoxError], Unit] = {
    Interpreter(stmts).swap.map { err =>
      Seq(LoxError(err.token.line, err.message))
    }.swap
  }

  private def run(source: String): Either[Seq[LoxError], Unit] = {
    for {
      tokens <- scan(source)
      expr <- parse(tokens)
      _ <- interpret(expr)
    } yield ()
  }

  private def runFile(path: String): Unit = {
    run(Source.fromFile(path, "UTF-8").mkString) match {
      case Left(errors) => errors.foreach(reportError); sys.exit(1)
      case Right(_) =>
    }
  }

  private def runPrompt(): Unit = {
    print("> ")
    Source.fromInputStream(System.in, Charset.defaultCharset.displayName).getLines().foreach { line =>
      run(line).fold(_.foreach(reportError), _ => ())
      print("> ")
    }
  }

  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0))
    case _ => println("Usage: slox [script]"); sys.exit(1)
  }
}
