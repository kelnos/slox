package org.spurint.slox

import org.spurint.slox.Interpreter.InterpreterError

trait LoxCallable {
  def name: String
  def arity: Int
  def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, LiteralValue[_]]
}
