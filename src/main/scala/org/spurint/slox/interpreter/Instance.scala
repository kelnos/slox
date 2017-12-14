package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token

trait Instance {
  protected def cls: LoxClassBase
  def name: String = cls.name
  def get(name: Token): Option[LiteralValue] = {
    cls.findGetter(this, name.lexeme)
      .orElse(cls.findMethod(this, name.lexeme))
  }
  def set(name: Token, value: LiteralValue): LiteralValue
}
