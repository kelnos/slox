package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Interpreter.{InterpreterError, RuntimeError}
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.scanner.Token

class LoxClass(nameToken: Token) extends LoxCallable {
  override def name: String = nameToken.lexeme
  override def arity: Int = 0
  override def line: Int = nameToken.line

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    Left(RuntimeError(nameToken, "Unimplemented"))
  }

  override lazy val toString: String = s"<cls $name>"
}
