package org.spurint.slox.interpreter.native

import java.io._
import org.spurint.slox.interpreter.LoxClass
import org.spurint.slox.interpreter.native.NativeClass.{ExceptionalError, NativeError}
import scala.util.Try

object FileClass extends NativeClass {
  override val name: String = "File"
  override protected[native] def _staticMethodNames: Set[String] = Set(
    "init",
    "exists",
  )
  override protected[native] def _instanceCls: Option[Class[_ <: NativeInstance]] = Some(classOf[FileClass])
  override protected[native] def _methodNames: Set[String] = Set(
    "readLine",
    "writeLine",
    "flush",
    "close",
  )

  override protected[native] def _getterNames: Set[String] = Set(
    "size",
  )

  def init(cls: LoxClass, filename: String, mode: String): Either[NativeError, FileClass] = {
    Try(new FileClass(cls, filename, mode)).toEither.swap.map(
      t => ExceptionalError(nameToken, t.getMessage)
    ).swap
  }

  def exists(filename: String): Boolean = new File(filename).exists()
}

class FileClass(override protected val cls: LoxClass, filename: String, mode: String) extends NativeInstance {
  override def name: String = "File"

  private val (reader, writer) = mode match {
    case "r" => (Some(new BufferedReader(new InputStreamReader(new FileInputStream(filename)))), None)
    case "w" => (None, Some(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename)))))
  }

  def readLine(): String = {
    reader.map(_.readLine()).orNull
  }

  def writeLine(line: String): Unit = {
    writer.foreach(_.write(line + "\n"))
  }

  def flush(): Unit = {
    writer.foreach(_.flush())
  }

  def size: Long = {
    Try(new File(filename).length()).getOrElse(0)
  }

  def close(): Unit = {
    reader.foreach(_.close())
    writer.foreach(_.close())
  }
}
