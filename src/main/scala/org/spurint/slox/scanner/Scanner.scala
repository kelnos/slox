package org.spurint.slox.scanner

import org.spurint.slox.model.LiteralValue._
import org.spurint.slox.scanner.Token.{ConstLexemeType, ConstLiteralType}
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
    Token.Type.Continue,
    Token.Type.Break,
    Token.Type.Try,
    Token.Type.Catch,
    Token.Type.Finally,
  )

  private val constLiteralTokenList = Seq[ConstLiteralType](
    Token.Type.False,
    Token.Type.Nil,
    Token.Type.True,
  )

  private val simpleTokens = constLexemeTokenList
    .map(t => t.lexeme ^^ (_ => Token(t, t.lexeme, literal = None)))
    .reduce((accum, next) => accum | next)

  private val simpleLiteralTokens = constLiteralTokenList
    .map(t => t.lexeme ^^ (_ => Token(t, t.lexeme, Some(t.literal))))
    .reduce((accum, next) => accum | next)

  private val identifier =
    "[a-zA-Z_][a-zA-Z_0-9]*".r ^^ (iden => Token(Token.Type.Identifier, iden, Some(IdentifierValue(iden))))

  private val string =
    """"[^"]*"""".r ^^ (s => Token(Token.Type.String, s, Some(StringValue(s.substring(1, s.length - 1)))))

  private val number =
    "[0-9]+(?:\\.[0-9]+)?".r ^^ (num => Token(Token.Type.Number, num, Some(NumberValue(num.toDouble))))

  private val singleLineComment =
    "//.*(?:\r\n|\r|\n)".r ^^ (com => Token(Token.Type.SingleLineComment, com.trim, Some(CommentValue(com.substring(2).trim))))

  private val invalidIdentifier =
    "[0-9]+[a-zA-Z_][a-zA-Z_0-9]*".r ^^ (inv => Token(Token.Type.Invalid, inv, None))

  private val invalid =
    "\\S+".r ^^ (inv => Token(Token.Type.Invalid, inv, None))

  private val tokens: Parser[List[Token]] = {
    phrase(rep(positioned(
      string | singleLineComment | simpleTokens | simpleLiteralTokens |
      invalidIdentifier | number | identifier | invalid
    )))
  }

  def apply(source: String): Seq[Token] = {
    (parse(tokens, source) match {
      case NoSuccess(msg, _) => Seq(Token(Token.Type.Invalid, msg, literal = None))
      case Success(result, _) => result
    }) :+ Token(Token.Type.Eof, Token.Type.Eof.lexeme, None)
  }
}