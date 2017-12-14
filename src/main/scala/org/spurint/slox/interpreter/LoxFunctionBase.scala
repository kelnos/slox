package org.spurint.slox.interpreter

import java.util.UUID
import org.spurint.slox.model.LiteralValue.ClassInstanceValue
import org.spurint.slox.model.LoxCallable
import org.spurint.slox.scanner.Token

trait LoxFunctionBase extends LoxCallable {
  protected def closure: Environment
  protected def withNewEnvironment(environment: Environment): LoxFunctionBase

  def bind(instance: Instance): LoxFunctionBase = {
    val boundEnvironment = closure.pushScope(s"fbind-$name-${UUID.randomUUID()}")
    val boundThisEnvironment = boundEnvironment.define(Token.thisToken(line), ClassInstanceValue(instance))
    withNewEnvironment(boundThisEnvironment)
  }
}
