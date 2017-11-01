package org.spurint.slox.parser

import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token

sealed trait Expr

object Expr {
  case class Assign(name: Token, value: Expr) extends Expr
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Call(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: LiteralValue[_]) extends Expr
  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
  case class Variable(name: Token) extends Expr
}