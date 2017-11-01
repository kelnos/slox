package org.spurint.slox.interpreter.native

import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.interpreter.Environment
import org.spurint.slox.model.LiteralValue.NumberValue
import org.spurint.slox.model.{LiteralValue, LoxCallable}

object ClockFunction extends LoxCallable {
  override val name: String = "clock"
  override val arity: Int = 0

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    Right(NumberValue(System.currentTimeMillis()) -> environment)
  }
}
