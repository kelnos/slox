package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue.{CallableValue, GettableValue}
import org.spurint.slox.util.HasLineInfo

trait LoxClassBase extends HasLineInfo {
  def name: String
  protected def superclass: Option[LoxClassBase]

  protected def methods: Map[String, LoxFunction]
  protected def getters: Map[String, LoxFunction]

  def findMethod(instance: LoxInstance, name: String): Option[CallableValue] = {
    findAndBindThing(methods, instance, name).map(CallableValue.apply)
      .orElse(superclass.flatMap(_.findMethod(instance, name)))
  }

  def findGetter(instance: LoxInstance, name: String): Option[GettableValue] = {
    findAndBindThing(getters, instance, name).map(GettableValue.apply)
      .orElse(superclass.flatMap(_.findGetter(instance, name)))
  }

  private def findAndBindThing(things: Map[String, LoxFunction], instance: LoxInstance, name: String): Option[LoxFunction] = {
    things.get(name).map(_.bind(instance))
  }
}
