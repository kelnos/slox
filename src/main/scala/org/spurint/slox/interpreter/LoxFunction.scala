package org.spurint.slox.interpreter

import java.util.UUID
import org.spurint.slox.interpreter.Interpreter.{InterpreterError, RuntimeError}
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.model.LiteralValue.{CallableValue, NilValue}
import org.spurint.slox.parser.Expr
import org.spurint.slox.scanner.Token
import org.spurint.slox.util.{EitherEnrichments, LoxLogger}

object LoxFunction {
  // hack alert: the closure we get below is actually the environment immediately before the symbol
  // for this function has been put into the environment.  this is unavoidable because of how we've
  // designed our immutable env.  so, hack it in here before calling the function -- this will fix
  // recursive calls.
  private def hackFunctionRefInto(name: Token, function: LoxFunction, environment: Environment): Environment = {
    environment.define(name, CallableValue(function))
  }
}

class LoxFunction(fname: Option[Token], private val declaration: Expr.Function, override protected val closure: Environment, resolvedLocals: Map[Int, Int], isInitializer: Boolean) extends LoxFunctionBase with LoxLogger {
  override def name: String = fname.map(_.lexeme).getOrElse(s"<fn@$line>")
  override def arity: Int = declaration.parameters.length
  override def line: Int = fname.map(_.line).getOrElse(declaration.line)

  private val hackedClosure = fname.map(LoxFunction.hackFunctionRefInto(_, this, closure)).getOrElse(closure)

  override protected def withNewEnvironment(environment: Environment): LoxFunctionBase = {
    new LoxFunction(fname, declaration, environment, resolvedLocals, isInitializer)
  }

  override def call(environment: Environment, arguments: Seq[LiteralValue]): Either[InterpreterError, (LiteralValue, Environment)] = {
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
      .map(returnEnv => (NilValue: LiteralValue, returnEnv))
      .recover { case Interpreter.Return(returnValue, returnEnv) => (returnValue, returnEnv) }
      .flatMap { case (returnValue, returnEnv) =>
        val actualReturnValue = if (isInitializer) {
          val thisToken = Token.thisToken(line)
          doublyHackedClosure.getAt(0, thisToken)
            .map(Right.apply)
            .getOrElse(Left(RuntimeError(thisToken, "BUG: Unable to find 'this' in class initializer scope")))
        } else {
          Right(returnValue)
        }

        actualReturnValue.map { arv =>
          // hack alert: for the same reason as the hack described above, we need to merge
          // any changes to the parent scopes during function execution back into the caller's
          // environment before passing it back.
          arv -> environment.mergeIn(returnEnv)
        }
      }
  }

  override lazy val toString: String = s"<fn $name>"
}
