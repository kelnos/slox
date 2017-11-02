package org.spurint.slox.interpreter

import java.util.UUID
import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.model.LiteralValue.NilValue
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.parser.Stmt
import org.spurint.slox.util._

class LoxFunction(declaration: Stmt.Function) extends LoxCallable {
  override val name: String = declaration.name.lexeme
  override val arity: Int = declaration.parameters.length

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    val newScopeId = s"call-$name-${UUID.randomUUID()}"
    val callEnvironment = declaration.parameters.zip(arguments).foldLeft(environment.pushScope(newScopeId)) {
      case (env, (param, arg)) => env.define(param.lexeme, arg)
    }
    Interpreter(declaration.body, callEnvironment)
      .map((NilValue: LiteralValue[_], _))
      .recover { case Interpreter.Return(returnValue, returnEnvironment) => returnValue -> returnEnvironment }
      .flatMap { case (returnValue, returnEnvironment) =>
        returnEnvironment.popScopeTo(environment.id).leftMap(interpreterScopeError).map(returnValue -> _)
      }
  }
}
