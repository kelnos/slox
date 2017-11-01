package org.spurint.slox.model

import org.spurint.slox.interpreter.Environment
import org.spurint.slox.interpreter.Interpreter.InterpreterError

trait LoxCallable {
  def name: String
  def arity: Int
  def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)]
}
