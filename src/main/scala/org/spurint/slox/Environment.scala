package org.spurint.slox

import org.spurint.slox.Environment.{ScopeError, UndefinedVariableError}

object Environment {
  sealed trait ScopeError
  case object ScopeError extends ScopeError
  case class UndefinedVariableError(variable: Token)

  def apply(): Environment = new Environment(Map.empty[String, LiteralValue[_]], enclosing = None)
}

class Environment private (values: Map[String, LiteralValue[_]], enclosing: Option[Environment]) {
  def define(name: String, value: LiteralValue[_]): Environment = new Environment(values + (name -> value), enclosing)

  def assign(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    if (values.contains(name.lexeme)) {
      Right(new Environment(values + (name.lexeme -> value), enclosing))
    } else {
      enclosing.map(_.assign(name, value)).getOrElse(Left(UndefinedVariableError(name)))
    }
  }

  def get(name: Token): Option[LiteralValue[_]] = {
    values.get(name.lexeme).orElse(enclosing.flatMap(enc => enc.get(name)))
  }

  def pushScope(): Environment = new Environment(Map.empty[String, LiteralValue[_]], Some(this))
  def popScope(): Either[ScopeError, Environment] = enclosing.map(Right.apply).getOrElse(Left(ScopeError))
}
