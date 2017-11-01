package org.spurint.slox.parser

import org.spurint.slox.parser.Expr._

trait AstPrinter[T] {
  def apply(printable: T): String
}

object AstPrinter {
  def apply[T](printable: T)(implicit printer: AstPrinter[T]): String = printer(printable)
}

object ExprAstPrinters {
  private def parenthesize[T <: Expr](name: String, exprs: T*)(implicit printable: AstPrinter[T]): String = {
    "(" + name + exprs.map(expr => " " + AstPrinter(expr)).mkString + ")"
  }

  implicit val assignPriner: AstPrinter[Assign] = (expr: Assign) => parenthesize(expr.name.lexeme, expr.value)
  implicit val unaryPrinter: AstPrinter[Unary] = (expr: Unary) => parenthesize(expr.operator.lexeme, expr.right)
  implicit val callPrinter: AstPrinter[Call] = (expr: Call) => parenthesize(genericExprPrinter(expr.callee), expr.arguments: _*)
  implicit val binaryPrinter: AstPrinter[Binary] = (expr: Binary) => parenthesize(expr.operator.lexeme, expr.left, expr.right)
  implicit val groupingPrinter: AstPrinter[Grouping] = (expr: Grouping) => parenthesize("group", expr.expression)
  implicit val literalPrinter: AstPrinter[Literal] = (expr: Literal) => expr.value.toString
  implicit val logicalPrinter: AstPrinter[Logical] = (expr: Logical) => parenthesize(expr.operator.lexeme, expr.left, expr.right)
  implicit val variablePrinter: AstPrinter[Variable] = (expr: Variable) => expr.name.lexeme

  implicit def genericExprPrinter: AstPrinter[Expr] = {
    case a: Assign => assignPriner(a)
    case u: Unary => unaryPrinter(u)
    case b: Binary => binaryPrinter(b)
    case c: Call => callPrinter(c)
    case g: Grouping => groupingPrinter(g)
    case l: Literal => literalPrinter(l)
    case l: Logical => logicalPrinter(l)
    case v: Variable => variablePrinter(v)
  }
}