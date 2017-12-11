package org.spurint.slox.interpreter

import org.spurint.slox.scanner.Token

class LoxMetaClass(nameToken: Token, protected val methods: Map[String, LoxFunction]) extends LoxClassBase {
  override val name: String = nameToken.lexeme
  override val line: Int = nameToken.line
  override protected val superclass: Option[LoxClassBase] = None
  override protected val getters: Map[String, LoxFunction] = Map.empty

  override lazy val toString: String = s"<cls $name>"
}

