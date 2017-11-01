package org.spurint.slox

import org.spurint.slox.LiteralValue.{BooleanValue, NilValue, NumberValue, StringValue}
import scala.annotation.tailrec

object Interpreter {
  case class InterpreterError(token: Token, message: String)

  def apply(stmts: Seq[Stmt]): Either[InterpreterError, Unit] = {
    @tailrec
    def rec(stmts: Seq[Stmt], environment: Environment): Either[InterpreterError, Environment] = {
      stmts match {
        case lastStmt :: Nil => execute(lastStmt, environment)
        case nextStmt :: moreStmts => execute(nextStmt, environment) match {
          case Right(environment1) => rec(moreStmts, environment1)
          case l => l
        }
        case Nil => Right(environment)
      }
    }
    rec(stmts, Environment()).map(_ => ())
  }

  private def execute(stmt: Stmt, environment: Environment): Either[InterpreterError, Environment] = {
    stmt match {
      case b: Stmt.Block => executeBlockStmt(b, environment)
      case e: Stmt.Expression => executeExpressionStmt(e, environment)
      case p: Stmt.Print => executePrintStmt(p, environment)
      case v: Stmt.Var => executeVarStmt(v, environment)
    }
  }

  private def executeBlockStmt(block: Stmt.Block, environment: Environment): Either[InterpreterError, Environment] = {
    block.statements.foldLeft[Either[InterpreterError, Environment]](Right(environment.pushScope())) {
      case (Right(curEnvironment), stmt) => execute(stmt, curEnvironment)
      case (l, _) => l
    }.map(_ => environment)
  }

  private def executeExpressionStmt(stmt: Stmt.Expression, environment: Environment): Either[InterpreterError, Environment] = {
    evaluate(stmt.expression, environment).map(_ => environment)
  }

  private def executePrintStmt(stmt: Stmt.Print, environment: Environment): Either[InterpreterError, Environment] = {
    for {
      res <- evaluate(stmt.expression, environment)
      (value, environment1) = res
    } yield {
      println(value.toString)
      environment1
    }
  }

  private def executeVarStmt(stmt: Stmt.Var, environment: Environment): Either[InterpreterError, Environment] = {
    for {
      res <- stmt.initializer.map(evaluate(_, environment)).getOrElse(Right(NilValue -> environment))
      (value, environment1) = res
    } yield {
      environment1.define(stmt.name.lexeme, value)
    }
  }

  private def evaluate(expr: Expr, environment: Environment): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    expr match {
      case l: Expr.Literal => evaluateLiteral(l, environment)
      case u: Expr.Unary => evaluateUnary(u, environment)
      case b: Expr.Binary => evaluateBinary(b, environment)
      case g: Expr.Grouping => evaluateGrouping(g, environment)
      case v: Expr.Variable => evaluateVariable(v, environment).map(_ -> environment)
      case a: Expr.Assign => evaluateAssign(a, environment)
    }
  }

  private def evaluateLiteral(literal: Expr.Literal, environment: Environment): Either[InterpreterError, (LiteralValue[_], Environment)] =
    Right(literal.value -> environment)

  private def evaluateUnary(unary: Expr.Unary, environment: Environment): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    evaluate(unary.right, environment).flatMap { case (right, environment1) =>
      unary.operator.`type` match {
        case Token.Type.Bang => Right(BooleanValue(!isTruthy(right)) -> environment1)
        case Token.Type.Minus => right match {
          case NumberValue(n) => Right(NumberValue(-n) -> environment1)
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

  private def evaluateBinary(binary: Expr.Binary, environment: Environment): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    for {
      res0 <- evaluate(binary.left, environment)
      (left, environment1) = res0
      res1 <- evaluate(binary.right, environment1)
      (right, environment2) = res1
      value <- evaluateBinary(binary, left, binary.operator, right).map(_ -> environment2)
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
        case Token.Type.Slash =>
          if (rn == 0) Left(InterpreterError(operator, "Division by zero"))
          else Right(NumberValue(ln / rn))
        case Token.Type.Plus => Right(NumberValue(ln + rn))
        case Token.Type.Minus => Right(NumberValue(ln - rn))
        case _ => Left(InterpreterError(operator, "Cannot perform arithmetic with this operator"))
      }
      case (StringValue(ls), StringValue(rs)) => operator.`type` match {
        case Token.Type.Plus => Right(StringValue(ls + rs))
        case _ => Left(InterpreterError(operator, "Cannot perform arithmetic on string values"))
      }
      case (StringValue(ls), _) => operator.`type` match {
        case Token.Type.Plus => Right(StringValue(ls + right.toString))
        case _ => Left(InterpreterError(operator, "Cannot perform arithmetic on string values"))
      }
      case (_, StringValue(rs)) => operator.`type` match {
        case Token.Type.Plus => Right(StringValue(left.toString + rs))
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

  private def evaluateGrouping(grouping: Expr.Grouping, environment: Environment): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    evaluate(grouping.expression, environment)
  }

  private def evaluateVariable(variable: Expr.Variable, environment: Environment): Either[InterpreterError, LiteralValue[_]] = {
    environment
      .get(variable.name)
      .map(Right.apply)
      .getOrElse(Left(InterpreterError(variable.name, s"Undefined variable")))
  }

  private def evaluateAssign(assign: Expr.Assign, environment: Environment): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    for {
      res <- evaluate(assign.value, environment)
      (value, environment1) = res
      environment2 <- environment1.assign(assign.name, value).swap.map { _ =>
        InterpreterError(assign.name, "Attempt to assign to an undefined variable")
      }.swap
    } yield {
      value -> environment2
    }
  }
}
