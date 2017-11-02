package org.spurint.slox.parser

import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token
import org.spurint.slox.util.HasLineInfo

sealed trait Expr extends HasLineInfo

object Expr {
  case class Assign(name: Token, value: Expr) extends Expr { val line: Int = name.line }
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr { val line: Int = operator.line }
  case class Call(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr { val line: Int = paren.line }
  case class Grouping(expression: Expr) extends Expr { val line: Int = expression.line}
  case class Literal(value: LiteralValue[_]) extends Expr { val line: Int = -1 }
  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr { val line: Int = operator.line }
  case class Unary(operator: Token, right: Expr) extends Expr { val line: Int = operator.line }
  case class Variable(name: Token) extends Expr { val line: Int = name.line }
}