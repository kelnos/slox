package org.spurint.slox.model

import org.spurint.slox.interpreter.Instance

sealed trait LiteralValue

object LiteralValue {
  case object NilValue extends LiteralValue { override val toString: String = "nil" }
  case class NumberValue(value: Double) extends LiteralValue { override val toString: String = fmtNumber(value) }
  case class StringValue(value: String) extends LiteralValue { override val toString: String = value }
  case class BooleanValue(value: Boolean) extends LiteralValue { override val toString: String = value.toString }
  case class IdentifierValue(value: String) extends LiteralValue { override val toString: String = value }
  case class CommentValue(value: String) extends LiteralValue { override val toString: String = value }
  case class CallableValue(value: LoxCallable) extends LiteralValue { override val toString: String = value.toString }
  case class ClassInstanceValue(value: Instance) extends LiteralValue { override val toString: String = value.toString }
  case class GettableValue(value: LoxCallable) extends LiteralValue { override val toString: String = value.toString }

  private def fmtNumber(number: Double): String = {
    if (number > Long.MaxValue || number < Long.MinValue || number.toLong != number) {
      number.toString
    } else {
      number.toLong.toString
    }
  }
}