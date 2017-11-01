package org.spurint.slox.parser

import org.spurint.slox.scanner.Token

sealed trait Stmt

object Stmt {
  case class Block(statements: Seq[Stmt]) extends Stmt
  case class Expression(expression: Expr) extends Stmt
  case class Function(name: Token, parameters: Seq[Token], body: Seq[Stmt]) extends Stmt
  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt
  case class Print(expression: Expr) extends Stmt
  case class Return(keyword: Token, value: Expr) extends Stmt
  case class Var(name: Token, initializer: Option[Expr]) extends Stmt
  case class While(condition: Expr, body: Stmt) extends Stmt
}