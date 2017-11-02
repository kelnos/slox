package org.spurint.slox

import java.nio.charset.Charset
import org.spurint.slox.interpreter.Interpreter.RuntimeError
import org.spurint.slox.interpreter.{Environment, Interpreter}
import org.spurint.slox.parser.{AstPrinter, Expr, RecursiveDescentParser, Stmt}
import org.spurint.slox.scanner.{Scanner, Token}
import org.spurint.slox.util._
import scala.io.Source

object Lox extends App {
  case class LoxError(line: Int, message: String)

  private def strForTokenType(`type`: Token.Type): String = {
    `type` match {
      case clt: Token.ConstLexemeType => s"'${clt.lexeme}'"
      case Token.Type.Identifier => "[identifier]"
      case Token.Type.String => "[string]"
      case Token.Type.Number => "[number]"
      case Token.Type.SingleLineComment => "[single line comment]"
      case Token.Type.Invalid => "[invalid token]"
    }
  }

  private def reportError(err: LoxError): Unit = {
    System.err.println(s"ERROR:${err.line}: ${err.message}")
  }

  private def dumpAst(expr: Expr): Unit = {
    import org.spurint.slox.parser.ExprAstPrinters._
    println(AstPrinter(expr))
  }

  private def scan(source: String): Either[Seq[LoxError], Seq[Token]] = {
    val tokens = Scanner(source)
    val invalidTokens = tokens.collect { case t @ Token(Token.Type.Invalid, _, _, _) => t }
    if (invalidTokens.nonEmpty) {
      Left(invalidTokens.map(token => LoxError(token.line, s"Invalid token: ${token.lexeme}")))
    } else {
      Right(tokens)
    }
  }

  private def parse(tokens: Seq[Token]): Either[Seq[LoxError], Seq[Stmt]] = {
    val finalTokens = tokens.filterNot(_.`type` == Token.Type.SingleLineComment)
    RecursiveDescentParser(finalTokens).leftMap { err =>
      val actual = err.actual.headOption.map(t => strForTokenType(t.`type`)).getOrElse("(unknown)")
      val line = err.actual.headOption.map(_.line).getOrElse(-1)
      val expected = err.expected.map(strForTokenType).mkString(", ")
      Seq(LoxError(line, s"Unexpected token $actual; expected $expected"))
    }
  }

  private def interpret(stmts: Seq[Stmt], initialEnvironment: Option[Environment]): Either[Seq[LoxError], Environment] = {
    Interpreter(stmts, initialEnvironment).leftMap {
      case RuntimeError(token, message) => Seq(LoxError(token.line, s"$message: ${token.lexeme}"))
      case Interpreter.Return(_, _) => Seq(LoxError(-1, s"BUG: Got Return error outside function call"))
    }
  }

  private def run(source: String, initialEnvironment: Option[Environment] = None): Either[Seq[LoxError], Environment] = {
    for {
      tokens <- scan(source)
      stmts <- parse(tokens)
      finalEnvironment <- interpret(stmts, initialEnvironment)
    } yield finalEnvironment
  }

  private def runFile(path: String): Unit = {
    run(Source.fromFile(path, "UTF-8").mkString) match {
      case Left(errors) => errors.foreach(reportError); sys.exit(1)
      case Right(_) =>
    }
  }

  private def runPrompt(): Unit = {
    print("> ")
    Source
      .fromInputStream(System.in, Charset.defaultCharset.displayName)
      .getLines()
      .foldLeft(Option.empty[Environment]) { (env, line) =>
        val nextEnv = run(line, env).fold({ errs => errs.foreach(reportError); env }, Option.apply)
        print("> ")
        nextEnv
      }
  }

  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0))
    case _ => println("Usage: slox [script]"); sys.exit(1)
  }
}
