package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token
import scala.collection.mutable

class LoxInstance(override protected val cls: LoxClassBase) extends Instance {
  // FIXME: this is the only place where we have mutable state and i need to fix that later
  private val fields = new mutable.HashMap[String, LiteralValue]

  override def get(name: Token): Option[LiteralValue] = {
    fields.get(name.lexeme)
      .orElse(super.get(name))
  }

  override def set(name: Token, value: LiteralValue): LiteralValue = {
    fields.put(name.lexeme, value)
    value
  }

  override lazy val toString: String = s"<${cls.name} $name>"
}
