package org.spurint.slox

import org.spurint.slox.LiteralValue._

object Token {
  sealed trait Type {
    def lexeme: String
    def literal: Option[LiteralValue[_]] = None

    def asToken(lineNum: Int): Token = Token(this, lexeme, literal, lineNum)
  }

  object Type {
    // Single-character tokens.
    case object LeftParen extends Type { val lexeme = "(" }
    case object RightParen extends Type { val lexeme = ")" }
    case object LeftBrace extends Type { val lexeme = "{" }
    case object RightBrace extends Type { val lexeme = "}" }
    case object Comma extends Type { val lexeme = "," }
    case object Dot extends Type { val lexeme = "." }
    case object Minus extends Type { val lexeme = "-" }
    case object Plus extends Type { val lexeme = "+" }
    case object Semicolon extends Type { val lexeme = ";" }
    case object Slash extends Type { val lexeme = "/" }
    case object Star extends Type { val lexeme = "*" }

    // One or two character tokens.
    case object Bang extends Type { val lexeme = "!" }
    case object BangEqual extends Type { val lexeme = "!=" }
    case object Equal extends Type { val lexeme = "=" }
    case object EqualEqual extends Type { val lexeme = "==" }
    case object Greater extends Type { val lexeme = ">" }
    case object GreaterEqual extends Type { val lexeme = ">=" }
    case object Less extends Type { val lexeme = "<" }
    case object LessEqual extends Type { val lexeme = "<=" }

    // Literals.
    case class Identifier(lexeme: scala.Predef.String) extends Type { override val literal = Some(IdentifierValue(lexeme)) }
    case class String(lexeme: scala.Predef.String) extends Type { override val literal = Some(StringValue(lexeme.substring(1, lexeme.length - 1))) }
    case class Number(lexeme: scala.Predef.String) extends Type { override val literal = Some(NumberValue(lexeme.toDouble)) }

    // Keywords.
    case object And extends Type { val lexeme = "and" }
    case object Class extends Type { val lexeme = "class" }
    case object Else extends Type { val lexeme = "else" }
    case object False extends Type { val lexeme = "false"; override val literal = Some(BooleanValue(false)) }
    case object Fun extends Type { val lexeme = "fun" }
    case object For extends Type { val lexeme = "for" }
    case object If extends Type { val lexeme = "if" }
    case object Nil extends Type { val lexeme = "nil"; override val literal = Some(NilValue) }
    case object Or extends Type { val lexeme = "or" }
    case object Print extends Type { val lexeme = "print" }
    case object Return extends Type { val lexeme = "return" }
    case object Super extends Type { val lexeme = "super" }
    case object This extends Type { val lexeme = "this" }
    case object True extends Type { val lexeme = "true"; override val literal = Some(BooleanValue(true)) }
    case object Var extends Type { val lexeme = "var" }
    case object While extends Type { val lexeme = "while" }

    case class SingleLineComment(lexeme: scala.Predef.String) extends Type { override val literal = Some(CommentValue(lexeme.substring(2).trim)) }
    case object CommentStart extends Type { val lexeme = "/*" }
    case object CommentEnd extends Type { val lexeme = "*/" }

    case object Eof extends Type { val lexeme = "EOF" }

    case class Invalid(lexeme: scala.Predef.String) extends Type { override val literal = Some(StringValue(lexeme)) }
  }
}

case class Token(`type`: Token.Type, lexeme: String, literal: Option[LiteralValue[_]], line: Int)
