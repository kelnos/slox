package org.spurint.slox.interpreter.native

import org.spurint.slox.interpreter.native.NativeClass.NativeError
import org.spurint.slox.interpreter.{LoxClass, LoxClassBase}

object ExceptionClass extends NativeClass {
  override def name: String = "Exception"

  override protected[native] def _staticMethodNames: Set[String] = Set(
    "init",
  )
  override protected[native] def _instanceCls: Option[Class[_ <: NativeInstance]] = Some(classOf[ExceptionClass])
  override protected[native] def _methodNames: Set[String] = Set()
  override protected[native] def _getterNames: Set[String] = Set(
    "line",
    "message",
  )

  def init(cls: LoxClass, line: Int, message: String): Either[NativeError, ExceptionClass] = {
    Right(new ExceptionClass(cls, line, message))
  }
}

class ExceptionClass(override protected val cls: LoxClassBase, val line: Int, val message: String) extends NativeInstance
