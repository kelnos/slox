package org.spurint.slox.interpreter.native

import org.spurint.slox.interpreter.LoxClass
import org.spurint.slox.model.LiteralValue.StringValue

class SystemClass(listCls: LoxClass, argv: Seq[String]) extends NativeClass {
  override val name: String = "System"
  override protected[native] val _staticMethodNames: Set[String] = Set(
    "getargv",
    "getenv",
    "exit",
  )
  override protected[native] val _instanceCls: Option[Class[_ <: NativeInstance]] = None
  override protected[native] val _methodNames: Set[String] = Set.empty

  def getargv(): ListClass = new ListClass(listCls, argv.map(StringValue.apply))

  def getenv(name: String): String = System.getenv(name)

  def exit(code: Int): Unit = sys.exit(code)
}
