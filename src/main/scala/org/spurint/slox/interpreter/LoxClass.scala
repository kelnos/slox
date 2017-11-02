package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.model.LiteralValue.{CallableValue, ClassInstanceValue}
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.scanner.Token

class LoxClass(nameToken: Token, methods: Map[String, CallableValue]) extends LoxCallable {
  override def name: String = nameToken.lexeme
  override def arity: Int = 0
  override def line: Int = nameToken.line

  def findMethod(name: String): Option[CallableValue] = methods.get(name)

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    val instance = new LoxInstance(this)
    Right((ClassInstanceValue(instance), environment))
  }

  override lazy val toString: String = s"<cls $name>"
}
