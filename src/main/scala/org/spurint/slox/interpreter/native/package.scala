package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue.CallableValue
import org.spurint.slox.model.LoxCallable

package object native {
  private val nativeFunctions = Seq[LoxCallable](
    ClockFunction
  )

  def registerNativeFunctions(environment: Environment): Environment = {
    nativeFunctions.foldLeft(environment)((env, func) => env.define(func, CallableValue(func)))
  }
}
