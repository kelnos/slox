package org.spurint.slox.native

import org.spurint.slox.Interpreter.InterpreterError
import org.spurint.slox.LiteralValue.NumberValue
import org.spurint.slox.{Environment, LiteralValue, LoxCallable}

object ClockFunction extends LoxCallable {
  override val name: String = "clock"
  override val arity: Int = 0

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, LiteralValue[_]] = {
    Right(NumberValue(System.currentTimeMillis()))
  }
}
