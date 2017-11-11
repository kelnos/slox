package org.spurint.slox.scanner

import org.spurint.slox.model.LiteralValue
import org.spurint.slox.model.LiteralValue.{BooleanValue, NilValue}
import org.spurint.slox.util.{HasIdentifier, HasLineInfo}
import scala.util.parsing.input.{Position, Positional}

object Token {
  sealed trait Type

  sealed trait ConstLexemeType extends Type {
    def lexeme: String
  }

  sealed trait ConstLiteralType extends ConstLexemeType {
    def literal: LiteralValue[_]
  }

  object Type {
    // Single-character tokens.
    case object LeftParen extends ConstLexemeType { val lexeme = "(" }
    case object RightParen extends ConstLexemeType { val lexeme = ")" }
    case object LeftBrace extends ConstLexemeType { val lexeme = "{" }
    case object RightBrace extends ConstLexemeType { val lexeme = "}" }
    case object Comma extends ConstLexemeType { val lexeme = "," }
    case object Dot extends ConstLexemeType { val lexeme = "." }
    case object Minus extends ConstLexemeType { val lexeme = "-" }
    case object Plus extends ConstLexemeType { val lexeme = "+" }
    case object Semicolon extends ConstLexemeType { val lexeme = ";" }
    case object Slash extends ConstLexemeType { val lexeme = "/" }
    case object Star extends ConstLexemeType { val lexeme = "*" }

    // One or two character tokens.
    case object Bang extends ConstLexemeType { val lexeme = "!" }
    case object BangEqual extends ConstLexemeType { val lexeme = "!=" }
    case object Equal extends ConstLexemeType { val lexeme = "=" }
    case object EqualEqual extends ConstLexemeType { val lexeme = "==" }
    case object Greater extends ConstLexemeType { val lexeme = ">" }
    case object GreaterEqual extends ConstLexemeType { val lexeme = ">=" }
    case object Less extends ConstLexemeType { val lexeme = "<" }
    case object LessEqual extends ConstLexemeType { val lexeme = "<=" }

    // Literals.
    case object Identifier extends Type
    case object String extends Type
    case object Number extends Type

    // Keywords.
    case object And extends ConstLexemeType { val lexeme = "and" }
    case object Break extends ConstLexemeType { val lexeme = "break" }
    case object Class extends ConstLexemeType { val lexeme = "class" }
    case object Continue extends ConstLexemeType { val lexeme = "continue" }
    case object Else extends ConstLexemeType { val lexeme = "else" }
    case object False extends ConstLiteralType { val lexeme = "false"; val literal = BooleanValue(false) }
    case object Fun extends ConstLexemeType { val lexeme = "fun" }
    case object For extends ConstLexemeType { val lexeme = "for" }
    case object If extends ConstLexemeType { val lexeme = "if" }
    case object Nil extends ConstLiteralType { val lexeme = "nil"; val literal = NilValue }
    case object Or extends ConstLexemeType { val lexeme = "or" }
    case object Print extends ConstLexemeType { val lexeme = "print" }
    case object Return extends ConstLexemeType { val lexeme = "return" }
    case object Super extends ConstLexemeType { val lexeme = "super" }
    case object This extends ConstLexemeType { val lexeme = "this" }
    case object True extends ConstLiteralType { val lexeme = "true"; val literal = BooleanValue(true) }
    case object Var extends ConstLexemeType { val lexeme = "var" }
    case object While extends ConstLexemeType { val lexeme = "while" }

    case object SingleLineComment extends Type
    case object CommentStart extends ConstLexemeType { val lexeme = "/*" }
    case object CommentEnd extends ConstLexemeType { val lexeme = "*/" }

    case object Eof extends ConstLexemeType { val lexeme = "EOF" }

    case object Invalid extends Type
  }

  def thisToken(line: Int): Token = genToken(Token.Type.This, Token.Type.This.lexeme, line)

  def dummyIdentifier(name: String, line: Int): Token = genToken(Token.Type.Identifier, name, line)

  private def genToken(`type`: Token.Type, lexeme: String, _line: Int) = {
    val token = Token(`type`, lexeme, literal = None)
    token.setPos(new Position {
      override def line: Int = _line
      override def column: Int = 1
      override protected def lineContents: String = lexeme
    })
    token
  }
}

case class Token(`type`: Token.Type, lexeme: String, literal: Option[LiteralValue[_]]) extends Positional with HasLineInfo with HasIdentifier {
  val id: String = lexeme
  lazy val line: Int = pos.line
}
