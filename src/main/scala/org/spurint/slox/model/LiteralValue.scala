package org.spurint.slox.model

sealed trait LiteralValue[T] {
  def value: T
}

object LiteralValue {
  case object NilValue extends LiteralValue[Any] { override val toString: String = "nil"; val value: Any = null }
  case class NumberValue(value: Double) extends LiteralValue[Double] { override val toString: String = fmtNumber(value) }
  case class StringValue(value: String) extends LiteralValue[String] { override val toString: String = value }
  case class BooleanValue(value: Boolean) extends LiteralValue[Boolean] { override val toString: String = value.toString }
  case class IdentifierValue(value: String) extends LiteralValue[String] { override val toString: String = value }
  case class CommentValue(value: String) extends LiteralValue[String] { override val toString: String = value }
  case class CallableValue(value: LoxCallable) extends LiteralValue[LoxCallable] { override val toString: String = s"<fn ${value.name}>" }

  private def fmtNumber(number: Double): String = {
    if (number > Long.MaxValue || number < Long.MinValue || number.toLong != number) {
      number.toString
    } else {
      number.toLong.toString
    }
  }
}