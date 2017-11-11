package org.spurint.slox

import org.spurint.slox.interpreter.Environment.ScopeError
import org.spurint.slox.interpreter.Interpreter.{InterpreterError, RuntimeError}
import org.spurint.slox.scanner.Token

package object interpreter {
  def interpreterScopeError(scopeError: ScopeError): InterpreterError = RuntimeError(
    scopeError.id
      .map(id => Token(Token.Type.Invalid, id.toString, None))
      .getOrElse(Token(Token.Type.Invalid, "[no scope id]", None)),
    s"BUG: Attempt to pop scope but can't pop"
  )
}
