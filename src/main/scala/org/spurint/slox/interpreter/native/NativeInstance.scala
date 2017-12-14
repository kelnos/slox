package org.spurint.slox.interpreter.native

import org.spurint.slox.interpreter.Instance
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token

trait NativeInstance extends Instance {
  override def set(name: Token, value: LiteralValue): LiteralValue = value
}
