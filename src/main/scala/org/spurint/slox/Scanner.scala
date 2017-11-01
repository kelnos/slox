package org.spurint.slox

import org.spurint.slox.LiteralValue._
import org.spurint.slox.Token.{ConstLexemeType, ConstLiteralType}
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Scanner extends RegexParsers {
  private val whiteSpaceStr = "\\s+"
  override val whiteSpace: Regex = s"$whiteSpaceStr".r
  override val skipWhitespace: Boolean = true

  private val constLexemeTokenList = Seq[ConstLexemeType](
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
    Token.Type.Fun,
    Token.Type.For,
    Token.Type.If,
    Token.Type.Or,
    Token.Type.Print,
    Token.Type.Return,
    Token.Type.Super,
    Token.Type.This,
    Token.Type.Var,
    Token.Type.While,
  )

  private val constLiteralTokenList = Seq[ConstLiteralType](
    Token.Type.False,
    Token.Type.Nil,
    Token.Type.True,
  )

  private def simpleTokens(lineNum: Int) = constLexemeTokenList
    .map(t => t.lexeme ^^ (_ => Token(t, t.lexeme, literal = None, lineNum)))
    .reduce((accum, next) => accum | next)

  private def simpleLiteralTokens(lineNum: Int) = constLiteralTokenList
    .map(t => t.lexeme ^^ (_ => Token(t, t.lexeme, Some(t.literal), lineNum)))
    .reduce((accum, next) => accum | next)

  private def identifier(lineNum: Int) =
    "[a-zA-Z_][a-zA-Z_0-9]*".r ^^ (iden => Token(Token.Type.Identifier, iden, Some(IdentifierValue(iden)), lineNum))

  private def string(lineNum: Int) =
    """"[^"]*"""".r ^^ (s => Token(Token.Type.String, s, Some(StringValue(s.substring(1, s.length - 1))), lineNum))

  private def number(lineNum: Int) =
    "[0-9]+(?:\\.[0-9]+)?".r ^^ (num => Token(Token.Type.Number, num, Some(NumberValue(num.toDouble)), lineNum))

  private def singleLineComment(lineNum: Int) =
    "//.*$".r ^^ (com => Token(Token.Type.SingleLineComment, com, Some(CommentValue(com.substring(2).trim)), lineNum))

  private def invalidIdentifier(lineNum: Int) =
    "[0-9]+[a-zA-Z_][a-zA-Z_0-9]*".r ^^ (inv => Token(Token.Type.Invalid, inv, None, lineNum))

  private def invalid(lineNum: Int) =
    "\\S+".r ^^ (inv => Token(Token.Type.Invalid, inv, None, lineNum))

  private def tokens(lineNum: Int): Parser[List[Token]] = {
    phrase(rep(
      string(lineNum) | singleLineComment(lineNum) | simpleTokens(lineNum) | simpleLiteralTokens(lineNum) |
      invalidIdentifier(lineNum) | number(lineNum) | identifier(lineNum) | invalid(lineNum)
    ))
  }

  def apply(source: String): Seq[Token] = {
    source.split("\r\n|\r|\n").zipWithIndex.flatMap { case (line, lineNum) =>
      parse(tokens(lineNum + 1), line) match {
        case NoSuccess(_, _) => Seq(Token(Token.Type.Invalid, line, literal = None, lineNum + 1))
        case Success(result, _) => result
      }
    } :+ Token(Token.Type.Eof, Token.Type.Eof.lexeme, None, -1)
  }
}