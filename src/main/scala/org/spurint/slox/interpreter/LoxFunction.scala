package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.model.LiteralValue.NilValue
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.parser.Stmt

class LoxFunction(declaration: Stmt.Function) extends LoxCallable {
  override val name: String = declaration.name.lexeme
  override val arity: Int = declaration.parameters.length

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, LiteralValue[_]] = {
    val callEnvironment = declaration.parameters.zip(arguments).foldLeft(environment.pushScope()) {
      case (env, (param, arg)) => env.define(param.lexeme, arg)
    }
    Interpreter(declaration.body, callEnvironment).map(_ => NilValue)
  }
}
