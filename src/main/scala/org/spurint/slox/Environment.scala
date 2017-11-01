package org.spurint.slox

import org.spurint.slox.Environment.UndefinedVariableError

object Environment {
  case class UndefinedVariableError(variable: Token)

  def apply(): Environment = new Environment(Map.empty[String, LiteralValue[_]])
}

class Environment private (values: Map[String, LiteralValue[_]]) {
  def define(name: String, value: LiteralValue[_]): Environment = new Environment(values + (name -> value))

  def assign(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    if (values.contains(name.lexeme)) {
      Right(new Environment(values + (name.lexeme -> value)))
    } else {
      Left(UndefinedVariableError(name))
    }
  }

  def get(name: Token): Option[LiteralValue[_]] = values.get(name.lexeme)
}
