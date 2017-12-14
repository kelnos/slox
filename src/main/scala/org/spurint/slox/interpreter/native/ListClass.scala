package org.spurint.slox.interpreter.native

import org.spurint.slox.interpreter.LoxClass
import org.spurint.slox.interpreter.native.NativeClass.InitializationError
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.model.LiteralValue.{ClassInstanceValue, NilValue}
import scala.collection.mutable
import scala.util.Try

object ListClass extends NativeClass {
  override def name: String = "List"
  override protected[native] def _staticMethodNames: Set[String] = Set(
    "init"
  )
  override protected[native] def _instanceCls: Option[Class[_ <: NativeInstance]] = Some(classOf[ListClass])
  override protected[native] def _methodNames: Set[String] = Set(
    "append",
    "extend",
    "insert",
    "pop",
    "get",
    "set",
    "length",
  )

  def init(cls: LoxClass, initialItems: LiteralValue): Either[InitializationError, ListClass] = {
    initialItems match {
      case NilValue => Right(new ListClass(cls, Seq.empty[LiteralValue]))
      case ClassInstanceValue(list: ListClass) => Right(new ListClass(cls, list.items))
      case x => Left(InitializationError(nameToken, s"Invalid argument passed to constructor: $x"))
    }
  }
}

class ListClass(override protected val cls: LoxClass, initialItems: Seq[LiteralValue]) extends NativeInstance {
  private[ListClass] val items = new mutable.ListBuffer[LiteralValue]()
  items.appendAll(initialItems)

  def append(value: LiteralValue): ListClass = {
    items += value
    this
  }

  def extend(values: ListClass): ListClass = {
    items ++= values.items
    this
  }

  def insert(idx: Int, value: LiteralValue): ListClass = {
    items.insert(idx, value)
    this
  }

  def pop(): LiteralValue = {
    Try(items.remove(items.length - 1)).getOrElse(NilValue)
  }

  def get(idx: Int): LiteralValue = {
    Try(items(idx)).getOrElse(NilValue)
  }

  def set(idx: Int, value: LiteralValue): ListClass = {
    items(idx) = value
    this
  }

  def length(): Int = items.length
}
