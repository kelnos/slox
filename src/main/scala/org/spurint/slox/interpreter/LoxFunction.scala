package org.spurint.slox.interpreter

import java.util.UUID
import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.model.LiteralValue.{CallableValue, NilValue}
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.parser.Stmt
import org.spurint.slox.util.{EitherEnrichments, LoxLogger}

object LoxFunction {
  // hack alert: the closure we get below is actually the environment immediately before the symbol
  // for this function has been put into the environment.  this is unavoidable because of how we've
  // designed our immutable env.  so, hack it in here before calling the function -- this will fix
  // recursive calls.
  private def hackFunctionRefInto(function: LoxFunction, environment: Environment): Environment = {
    environment.define(function.declaration.name, CallableValue(function))
  }
}

class LoxFunction(private val declaration: Stmt.Function, closure: Environment, resolvedLocals: Map[Int, Int]) extends LoxCallable with LoxLogger {
  override def name: String = declaration.name.lexeme
  override def arity: Int = declaration.parameters.length
  override def line: Int = declaration.line

  private val hackedClosure = LoxFunction.hackFunctionRefInto(this, closure)

  override def call(environment: Environment, arguments: Seq[LiteralValue[_]]): Either[InterpreterError, (LiteralValue[_], Environment)] = {
    debug(this, s"Calling $name(${arguments.mkString(", ")})")

    // hack alert: because our environments are immutable the parent scopes in the caller's
    // environment almost certainly won't match the parent scopes in the closure.  so we need
    // to update the closure with any changes to parent scopes
    val doublyHackedClosure = hackedClosure.mergeIn(environment)

    val newScopeId = s"call-$name-${UUID.randomUUID()}"
    val callEnvironment = declaration.parameters.zip(arguments).foldLeft(doublyHackedClosure.pushScope(newScopeId)) {
      case (env, (param, arg)) => env.define(param, arg)
    }

    Interpreter(declaration.body, callEnvironment, resolvedLocals)
      .map(returnEnv => (NilValue: LiteralValue[_], returnEnv))
      .recover { case Interpreter.Return(returnValue, returnEnv) => (returnValue, returnEnv) }
      .map { case (returnValue, returnEnv) =>
        // hack alert: for the same reason as the hack described above, we need to merge
        // any changes to the parent scopes during function execution back into the caller's
        // environment before passing it back.
        returnValue -> environment.mergeIn(returnEnv)
      }
  }
}
