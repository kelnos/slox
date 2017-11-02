package org.spurint.slox.interpreter.native

import org.spurint.slox.model.LoxCallable

trait NativeFunction extends LoxCallable {
  override lazy val toString: String = s"<native $name>"
}
