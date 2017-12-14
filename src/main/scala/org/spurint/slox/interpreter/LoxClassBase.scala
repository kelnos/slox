package org.spurint.slox.interpreter

import org.spurint.slox.model.LiteralValue.{CallableValue, GettableValue}
import org.spurint.slox.util.HasLineInfo

trait LoxClassBase extends HasLineInfo {
  def name: String
  protected def superclass: Option[LoxClassBase]

  protected def methods: Map[String, LoxFunctionBase]
  protected def getters: Map[String, LoxFunctionBase]

  def findMethod(instance: Instance, name: String): Option[CallableValue] = {
    findAndBindThing(methods, instance, name).map(CallableValue.apply)
      .orElse(superclass.flatMap(_.findMethod(instance, name)))
  }

  def findGetter(instance: Instance, name: String): Option[GettableValue] = {
    findAndBindThing(getters, instance, name).map(GettableValue.apply)
      .orElse(superclass.flatMap(_.findGetter(instance, name)))
  }

  private def findAndBindThing(things: Map[String, LoxFunctionBase], instance: Instance, name: String): Option[LoxFunctionBase] = {
    things.get(name).map(_.bind(instance))
  }
}
