package org.spurint.slox

sealed trait Expr

object Expr {
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: LiteralValue[_]) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
}