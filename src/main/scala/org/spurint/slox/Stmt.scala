package org.spurint.slox

sealed trait Stmt

object Stmt {
  case class Block(statements: Seq[Stmt]) extends Stmt
  case class Expression(expression: Expr) extends Stmt
  case class Print(expression: Expr) extends Stmt
  case class Var(name: Token, initializer: Option[Expr]) extends Stmt
}