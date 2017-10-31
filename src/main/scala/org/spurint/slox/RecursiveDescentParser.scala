package org.spurint.slox

import scala.annotation.tailrec

object RecursiveDescentParser {
  case class ParserError(expected: Seq[Token.Type], actual: Seq[Token])

  def apply(tokens: Seq[Token]): Either[ParserError, Expr] = {
    expression(tokens).flatMap { case (expr, tail) =>
      tail.headOption match {
        case Some(Token(Token.Type.Eof, _, _, _)) => Right(expr)
        case _ => Left(ParserError(Seq(Token.Type.Eof), tail))
      }
    }
  }

  @tailrec
  private def synchronize(tokens: Seq[Token]): Seq[Token] = {
    tokens.headOption match {
      case Some(token) =>
        token.`type` match {
          case Token.Type.Semicolon =>
            tokens.tail
          case Token.Type.Class |
               Token.Type.Fun |
               Token.Type.Var |
               Token.Type.For |
               Token.Type.If |
               Token.Type.While |
               Token.Type.Print |
               Token.Type.Return =>
            tokens
          case _ =>
            synchronize(tokens.tail)
        }
      case _ => tokens
    }
  }

  private def consume(`type`: Token.Type, tokens: Seq[Token]): Either[ParserError, Seq[Token]] = {
    tokens.headOption match {
      case Some(Token(`type`, _, _, _)) => Right(tokens.tail)
      case _ => Left(ParserError(Seq(`type`), tokens))
    }
  }

  // this is used only for error display, not actual program logic
  private lazy val primaryTokenTypes = Seq[Token.Type](
    Token.Type.False,
    Token.Type.True,
    Token.Type.Nil,
    Token.Type.String("\"[number]\""),
    Token.Type.String("\"[string]\""),
    Token.Type.LeftParen
  )

  @inline
  private def primary(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    tokens.headOption match {
      case Some(Token(Token.Type.False | Token.Type.True | Token.Type.Nil | _: Token.Type.Number | _: Token.Type.String, _, literal, _)) =>
        Right((Expr.Literal(literal), tokens.tail))
      case Some(token) if token.`type` == Token.Type.LeftParen =>
        for {
          res <- expression(tokens.tail)
          (expr, tail) = res
          finalTail <- consume(Token.Type.RightParen, tail)
        } yield {
          (Expr.Grouping(expr), finalTail)
        }
      case _ => Left(ParserError(primaryTokenTypes, tokens))
    }
  }

  @inline
  private def expression(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    equality(tokens)
  }

  private object unary {
    private val unaryOperators = Set[Token.Type](
      Token.Type.Bang,
      Token.Type.Minus,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(operator) if unaryOperators.contains(operator.`type`) =>
          unary(tokens.tail).map { case (right, tail) =>
            (Expr.Unary(operator, right), tail)
          }
        case _ => primary(tokens)
      }
    }
  }

  private object multiplication {
    private val multiplicationOperators = Set[Token.Type](
      Token.Type.Slash,
      Token.Type.Star,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      unary(tokens).flatMap { case (left, tail) => multiplicationRec(left, tail) }
    }

    @tailrec
    private def multiplicationRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(operator) if multiplicationOperators.contains(operator.`type`) =>
          unary(tokens.tail) match {
            case Right((right, tail)) => multiplicationRec(Expr.Binary(left, operator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object addition {
    private val additionOperators = Set[Token.Type](
      Token.Type.Minus,
      Token.Type.Plus,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      multiplication(tokens).flatMap { case (left, tail) => additionRec(left, tail) }
    }

    @tailrec
    private def additionRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(operator) if additionOperators.contains(operator.`type`) =>
          multiplication(tokens.tail) match {
            case Right((right, tail)) => additionRec(Expr.Binary(left, operator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object comparison {
    private val comparisonOperators = Set[Token.Type](
      Token.Type.Greater,
      Token.Type.GreaterEqual,
      Token.Type.Less,
      Token.Type.LessEqual,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      addition(tokens).flatMap { case (left, tail) => comparisonRec(left, tail) }
    }

    @tailrec
    private def comparisonRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(comparator) if comparisonOperators.contains(comparator.`type`) =>
          addition(tokens.tail) match {
            case Right((right, tail)) => comparisonRec(Expr.Binary(left, comparator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object equality {
    private val equalityOperators = Set[Token.Type](
      Token.Type.BangEqual,
      Token.Type.EqualEqual,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      comparison(tokens).flatMap { case (left, tail) => equalityRec(left, tail) }
    }

    @tailrec
    private def equalityRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(comparator) if equalityOperators.contains(comparator.`type`) =>
          comparison(tokens.tail) match {
            case Right((right, tail)) => equalityRec(Expr.Binary(left, comparator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }
}
