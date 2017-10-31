package org.spurint.slox

import org.spurint.slox.LiteralValue.{BooleanValue, NilValue, NumberValue, StringValue}

object Interpreter {
  case class InterpreterError(token: Token, message: String)

  def apply(expr: Expr): Either[InterpreterError, LiteralValue[_]] = {
    evaluate(expr)
  }

  private def evaluate(expr: Expr): Either[InterpreterError, LiteralValue[_]] = {
    expr match {
      case l: Expr.Literal => evaluateLiteral(l)
      case u: Expr.Unary => evaluateUnary(u)
      case b: Expr.Binary => evaluateBinary(b)
      case g: Expr.Grouping => evaluateGrouping(g)
    }
  }

  private def evaluateLiteral(literal: Expr.Literal): Either[InterpreterError, LiteralValue[_]] = Right(literal.value)

  private def evaluateUnary(unary: Expr.Unary): Either[InterpreterError, LiteralValue[_]] = {
    evaluate(unary.right).flatMap { right =>
      unary.operator.`type` match {
        case Token.Type.Bang => Right(BooleanValue(!isTruthy(right)))
        case Token.Type.Minus => right match {
          case NumberValue(n) => Right(NumberValue(-n))
          case _ => Left(InterpreterError(unary.operator, "Cannot negate a non-numeric value"))
        }
        case _ => Left(InterpreterError(unary.operator, "Cannot perform unary operation with this operator"))
      }
    }
  }

  private def isTruthy(value: LiteralValue[_]): Boolean = {
    value match {
      case NilValue => false
      case BooleanValue(b) => b
      case _ => true
    }
  }

  private def evaluateBinary(binary: Expr.Binary): Either[InterpreterError, LiteralValue[_]] = {
    for {
      left <- evaluate(binary.left)
      right <- evaluate(binary.right)
      value <- evaluateBinary(binary, left, binary.operator, right)
    } yield value
  }

  private def evaluateBinary(binary: Expr.Binary, left: LiteralValue[_], operator: Token, right: LiteralValue[_]): Either[InterpreterError, LiteralValue[_]] = {
    operator.`type` match {
      case Token.Type.EqualEqual => Right(BooleanValue(isEqual(left, right)))
      case Token.Type.BangEqual => Right(BooleanValue(!isEqual(left, right)))
      case Token.Type.LessEqual => compare(binary, left, right).map(cmp => BooleanValue(cmp <= 0))
      case Token.Type.Less => compare(binary, left, right).map(cmp => BooleanValue(cmp < 0))
      case Token.Type.GreaterEqual => compare(binary, left, right).map(cmp => BooleanValue(cmp >= 0))
      case Token.Type.Greater => compare(binary, left, right).map(cmp => BooleanValue(cmp > 0))
      case Token.Type.Star | Token.Type.Slash | Token.Type.Plus | Token.Type.Minus =>
        arithmetic(binary, left, operator, right)
      case Token.Type.And | Token.Type.Or => compareBoolean(binary, left, right).map(BooleanValue.apply)
      case _ => Left(InterpreterError(operator, "Cannot evaluate binary expression joined by this operator"))
    }
  }

  private def arithmetic(binary: Expr.Binary, left: LiteralValue[_], operator: Token, right: LiteralValue[_]): Either[InterpreterError, LiteralValue[_]] = {
    (left, right) match {
      case (NumberValue(ln), NumberValue(rn)) => operator.`type` match {
        case Token.Type.Star => Right(NumberValue(ln * rn))
        case Token.Type.Slash => Right(NumberValue(ln / rn))
        case Token.Type.Plus => Right(NumberValue(ln + rn))
        case Token.Type.Minus => Right(NumberValue(ln - rn))
        case _ => Left(InterpreterError(operator, "Cannot perform arithmetic with this operator"))
      }
      case (StringValue(ls), StringValue(rs)) => operator.`type` match {
        case Token.Type.Plus => Right(StringValue(ls + rs))
        case _ => Left(InterpreterError(operator, "Cannot perform arithmetic on string values"))
      }
      case _ => Left(InterpreterError(operator, "Cannot perform arithmetic on non-numeric values"))
    }
  }

  private def compare(binary: Expr.Binary, left: LiteralValue[_], right: LiteralValue[_]): Either[InterpreterError, Int] = {
    (left, right) match {
      case (ln: NumberValue, rn: NumberValue) => Right(ln.value.compare(rn.value))
      case _ => Left(InterpreterError(binary.operator, "Cannot compare non-numeric values"))
    }
  }

  private def compareBoolean(binary: Expr.Binary, left: LiteralValue[_], right: LiteralValue[_]): Either[InterpreterError, Boolean] = {
    (left, right) match {
      case (BooleanValue(lb), BooleanValue(rb)) => binary.operator.`type` match {
        case Token.Type.And => Right(lb && rb)
        case Token.Type.Or => Right(lb || rb)
        case _ => Left(InterpreterError(binary.operator, "Cannot do boolean comparison with this operator"))
      }
      case _ => Left(InterpreterError(binary.operator, "Cannot perform comparison on non-boolean values"))
    }
  }

  private def isEqual(left: LiteralValue[_], right: LiteralValue[_]): Boolean = {
    (left, right) match {
      case (NilValue, NilValue) => true
      case (NilValue, _) => false
      case _ => left.equals(right)
    }
  }

  private def evaluateGrouping(grouping: Expr.Grouping): Either[InterpreterError, LiteralValue[_]] = {
    evaluate(grouping.expression)
  }
}
