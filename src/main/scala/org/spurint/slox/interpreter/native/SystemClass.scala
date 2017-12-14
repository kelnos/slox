package org.spurint.slox.interpreter.native

object SystemClass extends NativeClass {
  override val name: String = "System"
  override protected[native] val _staticMethodNames: Set[String] = Set(
    "getenv",
    "exit",
  )
  override protected[native] val _instanceCls: Option[Class[_ <: NativeInstance]] = None
  override protected[native] val _methodNames: Set[String] = Set.empty

  def getenv(name: String): String = System.getenv(name)

  def exit(code: Int): Unit = sys.exit(code)
}
