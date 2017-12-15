package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue.CallableValue
import org.spurint.slox.model.LoxCallable
import org.spurint.slox.scanner.Token

package object native {
  private val nativeFunctions = Seq[LoxCallable](
    ClockFunction
  )

  private val nativeClasses = Seq[(NativeClass, Option[Class[_ <: NativeInstance]])](
    (ExceptionClass, Some(classOf[ExceptionClass])),
    (ListClass, Some(classOf[ListClass])),
    (FileClass, Some(classOf[FileClass])),
  )

  def registerNativeFunctions(argv: Seq[String], environment: Environment): Environment = {
    val funcEnv = nativeFunctions.foldLeft(environment)((env, func) => env.define(func, CallableValue(func)))
    val clsEnv = nativeClasses.foldLeft(funcEnv) { case (env, (companion, nc)) =>
      env.define(Token.dummyIdentifier(companion.name, 0), CallableValue(NativeClass(companion, nc, env)))
    }
    clsEnv.getAtRoot(Token.dummyIdentifier("List", 0)) match {
      case Some(CallableValue(listCls: LoxClass)) =>
        val systemCls = new SystemClass(listCls, argv)
        clsEnv.define(Token.dummyIdentifier(systemCls.name, 0), CallableValue(NativeClass(systemCls, None, clsEnv)))
      case _ =>
        throw new RuntimeException("BUG: cannot find correct 'List' class in global env")
    }
  }
}
