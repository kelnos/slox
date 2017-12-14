package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue.CallableValue
import org.spurint.slox.model.LoxCallable
import org.spurint.slox.scanner.Token

package object native {
  private val nativeFunctions = Seq[LoxCallable](
    ClockFunction
  )

  private val nativeClasses = Seq[(NativeClass, Option[Class[_ <: NativeInstance]])](
    (SystemClass, None),
    (FileClass, Some(classOf[FileClass])),
  )

  def registerNativeFunctions(environment: Environment): Environment = {
    val funcEnv = nativeFunctions.foldLeft(environment)((env, func) => env.define(func, CallableValue(func)))
    nativeClasses.foldLeft(funcEnv) { case (env, (companion, nc)) =>
      env.define(Token.dummyIdentifier(companion.name, 0), CallableValue(NativeClass(companion, nc, env)))
    }
  }
}
