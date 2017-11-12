package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.model.LiteralValue.{CallableValue, ClassInstanceValue}
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.scanner.Token

class LoxClass(nameToken: Token,
               metaclass: LoxMetaClass,
               protected val methods: Map[String, LoxFunction],
               protected val getters: Map[String, LoxFunction])
  extends LoxInstance(metaclass)
  with LoxClassBase
  with LoxCallable
{
  override def name: String = nameToken.lexeme
  override lazy val arity: Int = methods.get("init").map(_.arity).getOrElse(0)
  override def line: Int = nameToken.line

  override def call(environment: Environment, arguments: Seq[LiteralValue]): Either[InterpreterError, (LiteralValue, Environment)] = {
    val instance = ClassInstanceValue(new LoxInstance(this))
    findMethod(instance.value, "init").map { case CallableValue(initializer) =>
      initializer.call(environment, arguments)
    }.getOrElse(Right(instance -> environment)).map { case (returnValue, finalEnvironment) =>
      (returnValue, finalEnvironment)
    }
  }

  override lazy val toString: String = s"<cls $name>"
}
