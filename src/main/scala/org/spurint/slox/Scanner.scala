package org.spurint.slox

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Scanner extends RegexParsers {
  private val whiteSpaceStr = "\\s+"
  override val whiteSpace: Regex = s"$whiteSpaceStr".r
  override val skipWhitespace: Boolean = true

  private def simpleTokens = Seq(
    Token.Type.CommentStart,
    Token.Type.CommentEnd,
    Token.Type.LeftParen,
    Token.Type.RightParen,
    Token.Type.LeftBrace,
    Token.Type.RightBrace,
    Token.Type.Comma,
    Token.Type.Dot,
    Token.Type.Minus,
    Token.Type.Plus,
    Token.Type.Semicolon,
    Token.Type.Slash,
    Token.Type.Star,
    Token.Type.BangEqual,
    Token.Type.Bang,
    Token.Type.EqualEqual,
    Token.Type.Equal,
    Token.Type.GreaterEqual,
    Token.Type.Greater,
    Token.Type.LessEqual,
    Token.Type.Less,
    Token.Type.And,
    Token.Type.Class,
    Token.Type.Else,
    Token.Type.False,
    Token.Type.Fun,
    Token.Type.For,
    Token.Type.If,
    Token.Type.Nil,
    Token.Type.Or,
    Token.Type.Print,
    Token.Type.Return,
    Token.Type.Super,
    Token.Type.This,
    Token.Type.True,
    Token.Type.Var,
    Token.Type.While,
  ).map(t => t.lexeme ^^ (_ => t)).reduce((accum, next) => accum | next)

  private def identifier = "[a-zA-Z_][a-zA-Z_0-9]*".r ^^ Token.Type.Identifier.apply
  private def string = """"[^"]*"""".r ^^ Token.Type.String.apply
  private def number = "[0-9]+(?:\\.[0-9]+)?".r ^^ (num => Token.Type.Number(num))
  private def singleLineComment = "//.*$".r ^^ Token.Type.SingleLineComment.apply

  private def invalidIdentifier = "[0-9]+[a-zA-Z_][a-zA-Z_0-9]*".r ^^ Token.Type.Invalid.apply
  private def invalid = "\\S+".r ^^ Token.Type.Invalid.apply

  private def tokens: Parser[List[Token.Type]] = {
    phrase(rep(string | singleLineComment | simpleTokens | invalidIdentifier | number | identifier | invalid))
  }

  def apply(source: String): Seq[Token] = {
    source.split("\r\n|\r|\n").zipWithIndex.flatMap { case (line, lineNum) =>
      parse(tokens, line) match {
        case NoSuccess(_, _) => Seq(Token(Token.Type.Invalid(line), line, literal = None, lineNum + 1))
        case Success(result, _) => result.map(_.asToken(lineNum + 1))
      }
    } :+ Token.Type.Eof.asToken(-1)
  }
}