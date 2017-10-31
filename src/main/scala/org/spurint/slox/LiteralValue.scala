package org.spurint.slox

sealed trait LiteralValue[T] {
  def value: T
}

object LiteralValue {
  case object NilValue extends LiteralValue[Any] { override val toString: String = "nil"; val value: Any = null }
  case class NumberValue(value: Double) extends LiteralValue[Double] { override val toString: String = value.toString}
  case class StringValue(value: String) extends LiteralValue[String] { override val toString: String = value }
  case class BooleanValue(value: Boolean) extends LiteralValue[Boolean] { override val toString: String = value.toString }
  case class IdentifierValue(value: String) extends LiteralValue[String] { override val toString: String = value }
  case class CommentValue(value: String) extends LiteralValue[String] { override val toString: String = value }
}