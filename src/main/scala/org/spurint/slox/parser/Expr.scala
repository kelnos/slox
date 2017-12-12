package org.spurint.slox.parser

import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token
import org.spurint.slox.util.HasLineInfo

sealed trait Expr extends HasLineInfo

object Expr {
  case class Assign(name: Token, value: Expr) extends Expr { val line: Int = name.line }
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr { val line: Int = operator.line }
  case class Call(callee: Expr, paren: Token, arguments: Seq[Expr]) extends Expr { val line: Int = paren.line }
  case class Function(locationToken: Token, parameters: Seq[Token], body: Seq[Stmt]) extends Expr { val line: Int = locationToken.line }
  case class Get(obj: Expr, name: Token) extends Expr { val line: Int = name.line }
  case class Grouping(expression: Expr) extends Expr { val line: Int = expression.line}
  case class Literal(value: LiteralValue) extends Expr { val line: Int = -1 }
  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr { val line: Int = operator.line }
  case class Set(obj: Expr, name: Token, value: Expr) extends Expr { val line: Int = name.line }
  case class Super(keyword: Token, method: Token) extends Expr { val line: Int = keyword.line }
  case class This(keyword: Token) extends Expr { val line: Int = keyword.line }
  case class Unary(operator: Token, right: Expr) extends Expr { val line: Int = operator.line }
  case class Variable(name: Token) extends Expr { val line: Int = name.line }
}