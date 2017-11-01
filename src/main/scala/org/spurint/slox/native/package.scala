package org.spurint.slox

import org.spurint.slox.LiteralValue.CallableValue

package object native {
  private val nativeFunctions = Seq[LoxCallable](
    ClockFunction
  )

  def registerNativeFunctions(environment: Environment): Environment = {
    nativeFunctions.foldLeft(environment)((env, func) => env.define(func.name, CallableValue(func)))
  }
}
