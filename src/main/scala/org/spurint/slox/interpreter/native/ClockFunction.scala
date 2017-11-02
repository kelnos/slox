package org.spurint.slox.interpreter.native

import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.interpreter.Environment
import org.spurint.slox.model.LiteralValue.NumberValue
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.util.LoxLogger

object ClockFunction extends LoxCallable with LoxLogger {
  override def name: String = "clock"
  override def arity: Int = 0
  override def line: Int = 0

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    debug(this, s"Calling $name()")
    Right(NumberValue(System.currentTimeMillis()) -> environment)
  }
}
