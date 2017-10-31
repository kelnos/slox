package org.spurint.slox

import org.spurint.slox.Expr.{Binary, Grouping, Literal, Unary}

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

  implicit val unaryPrinter: AstPrinter[Unary] = (expr: Unary) => parenthesize(expr.operator.lexeme, expr.right)
  implicit val binaryPrinter: AstPrinter[Binary] = (expr: Binary) => parenthesize(expr.operator.lexeme, expr.left, expr.right)
  implicit val groupingPrinter: AstPrinter[Grouping] = (expr: Grouping) => parenthesize("group", expr.expression)
  implicit val literalPrinter: AstPrinter[Literal] = (expr: Literal) => expr.value.toString

  implicit val genericExprPrinter: AstPrinter[Expr] = {
    case u: Unary => unaryPrinter(u)
    case b: Binary => binaryPrinter(b)
    case g: Grouping => groupingPrinter(g)
    case l: Literal => literalPrinter(l)
  }
}