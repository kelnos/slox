package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Environment.{ScopeError, UndefinedVariableError}
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token
import org.spurint.slox.util._
import scala.annotation.tailrec

object Environment {
  case class ScopeError(id: Option[String] = None)
  case class UndefinedVariableError(variable: Token)

  val global: Environment = {
    val env = Environment("GLOBAL")
    native.registerNativeFunctions(env)
  }

  def apply(id: String): Environment = new Environment(id, Map.empty[String, LiteralValue[_]], enclosing = None)
}

class Environment private (val id: String, private val values: Map[String, LiteralValue[_]], private val enclosing: Option[Environment]) {
  def define(name: String, value: LiteralValue[_]): Environment = {
    new Environment(id, values + (name -> value), enclosing)
  }

  def assign(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    assignHere(name, value).recoverWith { case _ =>
      enclosing
        .map(_.assign(name, value).map(newEnclosing => new Environment(id, values, Option(newEnclosing))))
        .getOrElse(Left(UndefinedVariableError(name)))
    }
  }

  def assignHere(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    if (values.contains(name.lexeme)) {
      Right(new Environment(id, values + (name.lexeme -> value), enclosing))
    } else {
      Left(UndefinedVariableError(name))
    }
  }

  def assignAt(distance: Int, name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    ancestor(distance).map(_.assignHere(name, value)).getOrElse(Left(UndefinedVariableError(name)))
  }

  def assignAtRoot(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    root.assignHere(name, value)
  }

  def get(name: Token): Option[LiteralValue[_]] = {
    getHere(name.lexeme).orElse(enclosing.flatMap(enc => enc.get(name)))
  }

  def getHere(name: String): Option[LiteralValue[_]] = {
    values.get(name)
  }

  def getAt(distance: Int, name: String): Option[LiteralValue[_]] = {
    ancestor(distance).flatMap(_.getHere(name))
  }

  def getAtRoot(name: String): Option[LiteralValue[_]] = {
    root.values.get(name)
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

  def push(environment: Environment): Environment = new Environment(environment.id, environment.values, Some(this))

  lazy val top: Environment = new Environment(id, values, enclosing = None)

  private def ancestor(distance: Int): Option[Environment] = {
    0.until(distance).foldLeft(Option(this))(
      (lastEnv, _) => lastEnv.flatMap(_.enclosing)
    )
  }

  private lazy val root: Environment = {
    @tailrec
    def rec(environment: Environment): Environment = {
      environment.enclosing match {
        case Some(enc) => rec(enc)
        case _ => environment
      }
    }
    rec(this)
  }
}
