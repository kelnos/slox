package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Environment.{ScopeError, UndefinedVariableError}
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token

object Environment {
  case class ScopeError(id: Option[String] = None)
  case class UndefinedVariableError(variable: Token)

  val global: Environment = {
    val env = Environment("GLOBAL")
    native.registerNativeFunctions(env)
  }

  def apply(id: String): Environment = new Environment(id, Map.empty[String, LiteralValue[_]], enclosing = None)
}

class Environment private (val id: String, values: Map[String, LiteralValue[_]], enclosing: Option[Environment]) {
  def define(name: String, value: LiteralValue[_]): Environment = {
    new Environment(id, values + (name -> value), enclosing)
  }

  def assign(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    if (values.contains(name.lexeme)) {
      Right(new Environment(id, values + (name.lexeme -> value), enclosing))
    } else {
      enclosing.map(
        _.assign(name, value).map(newEnclosing => new Environment(id, values, Option(newEnclosing)))
      ).getOrElse(Left(UndefinedVariableError(name)))
    }
  }

  def get(name: Token): Option[LiteralValue[_]] = {
    values.get(name.lexeme).orElse(enclosing.flatMap(enc => enc.get(name)))
  }

  def pushScope(id: String): Environment = {
    new Environment(id, Map.empty[String, LiteralValue[_]], Some(this))
  }

  def popScopeTo(id: String): Either[ScopeError, Environment] = {
    if (this.id ==  id) {
      Right(this)
    } else {
      enclosing.map(_.popScopeTo(id)).getOrElse(Left(ScopeError(Option(id))))
    }
  }
}
