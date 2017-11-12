package org.spurint.slox.model

import org.spurint.slox.interpreter.Environment
import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.util.{HasIdentifier, HasLineInfo}

trait LoxCallable extends HasLineInfo with HasIdentifier {
  override val id: String = name
  def name: String
  def arity: Int
  def call(environment: Environment, arguments: Seq[LiteralValue]): Either[InterpreterError, (LiteralValue, Environment)]
}
