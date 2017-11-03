package org.spurint.slox

import ch.qos.logback.classic.Level
import java.nio.charset.Charset
import org.slf4j.{Logger, LoggerFactory}
import org.spurint.slox.interpreter.Interpreter.RuntimeError
import org.spurint.slox.interpreter.{Environment, Interpreter}
import org.spurint.slox.parser.{RecursiveDescentParser, Stmt}
import org.spurint.slox.resolver.Resolver
import org.spurint.slox.scanner.{Scanner, Token}
import org.spurint.slox.util._
import scala.io.Source

object Lox extends App with LoxLogger {
  case class LoxError(line: Int, message: String) extends HasLineInfo

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
    error(err, err.message)
  }

  private def scan(source: String): Seq[Token] = {
    Scanner(source)
  }

  private def stripComments(tokens: Seq[Token]): Either[LoxError, Seq[Token]] = {
    tokens.foldLeft[Either[LoxError, (Int, Seq[Token])]](Right((0, Seq.empty[Token]))) {
      case (accum @ Right(_), Token(Token.Type.SingleLineComment, _, _ , _)) => accum
      case (Right((commentBlocks, ts)), Token(Token.Type.CommentStart, _, _, _)) => Right(commentBlocks + 1, ts)
      case (Right((commentBlocks, ts)), token @ Token(Token.Type.CommentEnd, _, _, _)) =>
        if (commentBlocks - 1 < 0) {
          Left(LoxError(token.line, "Invalid comment block nesting"))
        } else {
          Right((commentBlocks - 1, ts))
        }
      case (Right((commentBlocks, ts)), token) if commentBlocks == 0 => Right((commentBlocks, ts :+ token))
      case (accum @ Right(_), _) => accum
      case (l @ Left(_), _) => l
    }.map { case (_, ts) => ts }
  }

  private def validateTokens(tokens: Seq[Token]): Either[Seq[LoxError], Seq[Token]] = {
    val errors = tokens.collect {
      case token @ Token(Token.Type.Invalid, _, _, _) =>
        LoxError(token.line, s"Invalid sequence: ${token.lexeme}")
    }
    if (errors.nonEmpty) Left(errors)
    else Right(tokens)
  }

  private def parse(tokens: Seq[Token]): Either[LoxError, Seq[Stmt]] = {
    RecursiveDescentParser(tokens).leftMap { err =>
      val actual = err.actual.headOption.map(t => strForTokenType(t.`type`)).getOrElse("(unknown)")
      val line = err.actual.headOption.map(_.line).getOrElse(-1)
      val expected = err.expected.map(strForTokenType).mkString(", ")
      LoxError(line, s"Unexpected token $actual; expected $expected")
    }
  }

  private def resolve(stmts: Seq[Stmt], initialResolvedLocals: Map[Int, Int]): Either[LoxError, Map[Int, Int]] = {
    Resolver(stmts, initialResolvedLocals).leftMap(err => LoxError(err.token.line, err.message))
  }

  private def interpret(stmts: Seq[Stmt],
                        initialEnvironment: Option[Environment],
                        resolvedLocals: Map[Int, Int]): Either[LoxError, Environment] =
  {
    Interpreter(stmts, initialEnvironment, resolvedLocals).leftMap {
      case RuntimeError(token, message) => LoxError(token.line, s"$message: ${token.lexeme}")
      case _: Interpreter.ControlFlowChange => LoxError(-1, s"BUG: Got control flow change outside function call")
    }
  }

  private def run(source: String,
                  initialEnvironment: Option[Environment] = None,
                  initialResolvedLocals: Map[Int, Int] = Map.empty[Int, Int]): Either[Seq[LoxError], (Environment, Map[Int, Int])] =
  {
    for {
      tokens <- Right(scan(source))
      strippedTokens <- stripComments(tokens).leftMap(_ :: Nil)
      finalTokens <- validateTokens(strippedTokens)
      stmts <- parse(finalTokens).leftMap(_ :: Nil)
      resolvedLocals <- resolve(stmts, initialResolvedLocals).leftMap(_ :: Nil)
      finalEnvironment <- interpret(stmts, initialEnvironment, resolvedLocals).leftMap(_ :: Nil)
    } yield (finalEnvironment, resolvedLocals)
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
      .foldLeft((Option.empty[Environment], Map.empty[Int, Int])) { case ((env, loc), line) =>
        val nextState = run(line, env, loc).fold(
          { errs => errs.foreach(reportError); (env, loc) },
          { case (nextEnv, newLoc) => Option(nextEnv) -> newLoc }
        )
        print("> ")
        nextState
      }
  }

  Option(System.getenv("LOG_LEVEL")).map(Level.valueOf).foreach { logLevel =>
    LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME) match {
      case ll: ch.qos.logback.classic.Logger => ll.setLevel(logLevel)
      case x => warn(s"Unable to set log level because it's not a logback logger (${x.getClass.getName})")
    }
  }

  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0))
    case _ => error("Usage: slox [script]"); sys.exit(1)
  }
}
