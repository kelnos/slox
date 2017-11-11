package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Interpreter.{InterpreterError, RuntimeError}
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token
import scala.collection.mutable

class LoxInstance(cls: LoxClassBase) {
  // FIXME: this is the only place where we have mutable state and i need to fix that later
  private val fields = new mutable.HashMap[String, LiteralValue[_]]

  def name: String = cls.name

  def get(name: Token): Either[InterpreterError, LiteralValue[_]] = {
    fields.get(name.lexeme)
      .orElse(cls.findGetter(this, name.lexeme))
      .orElse(cls.findMethod(this, name.lexeme))
      .map(Right.apply)
      .getOrElse(Left(RuntimeError(name, s"Undefined property '${name.lexeme}'.")))
  }

  def set(name: Token, value: LiteralValue[_]): Either[InterpreterError, LiteralValue[_]] = {
    fields.put(name.lexeme, value)
    Right(value)
  }

  override lazy val toString: String = s"<${cls.name} $name>"
}
