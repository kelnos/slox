package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue.CallableValue
import org.spurint.slox.util.HasLineInfo

trait LoxClassBase extends HasLineInfo {
  def name: String

  protected def methods: Map[String, LoxFunction]

  def findMethod(instance: LoxInstance, name: String): Option[CallableValue] = {
    methods.get(name).map(_.bind(instance)).map(CallableValue.apply)
  }
}
