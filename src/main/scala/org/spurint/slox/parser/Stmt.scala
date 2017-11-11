package org.spurint.slox.parser

import org.spurint.slox.scanner.Token
import org.spurint.slox.util.HasLineInfo

sealed trait Stmt extends HasLineInfo

object Stmt {
  case class Block(statements: Seq[Stmt]) extends Stmt { val line: Int = statements.headOption.map(_.line).getOrElse(-1) }
  case class Break(keyword: Token) extends Stmt { val line: Int = keyword.line }
  case class Continue(keyword: Token) extends Stmt { val line: Int = keyword.line }
  case class Class(name: Token, staticMethods: Seq[Stmt.Function], methods: Seq[Stmt.Function], getters: Seq[Stmt.Function]) extends Stmt { val line: Int = name.line }
  case class Expression(expression: Expr) extends Stmt { val line: Int = expression.line }
  case class For(initializer: Option[Stmt], condition: Expr, increment: Option[Stmt], body: Stmt) extends Stmt { val line: Int = condition.line }
  case class Function(name: Token, function: Expr.Function) extends Stmt { val line: Int = name.line }
  case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt { val line: Int = condition.line }
  case class Print(expression: Expr) extends Stmt { val line: Int = expression.line }
  case class Return(keyword: Token, value: Expr) extends Stmt { val line: Int = keyword.line }
  case class Var(name: Token, initializer: Option[Expr]) extends Stmt { val line: Int = name.line }
}