package org.spurint.slox.interpreter.native

import java.lang.reflect.Method
import org.spurint.slox.interpreter.Interpreter.InterpreterError
import org.spurint.slox.interpreter._
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.model.LiteralValue._
import org.spurint.slox.scanner.Token
import org.spurint.slox.util.LoxLogger
import scala.util.{Failure, Success, Try}

object NativeClass {
  trait NativeError {
    def name: Token
    def message: String
  }
  case class RuntimeError(name: Token, message: String) extends NativeError
  case class ExceptionalError(name: Token, message: String) extends NativeError

  class NativeClassFunction(method: Method, override protected val closure: Environment, staticInstance: Option[NativeClass]) extends LoxFunctionBase with LoxLogger {
    override val name: String = method.getName
    override val arity: Int = if (name == "init") method.getParameterCount - 1 else method.getParameterCount
    override val line: Int = 0

    debug(s"New function named $name")

    private lazy val token = Token.dummyIdentifier(name, line)

    private def fetchInstance(environment: Environment): Either[InterpreterError, AnyRef] = {
      staticInstance.map(Right.apply).getOrElse(
        environment.getAt(0, Token.thisToken(0)) match {
          case Some(ClassInstanceValue(instance)) => Right(instance)
          case Some(x) => Left(Interpreter.RuntimeError(token, s"BUG: invalid type for 'this' pointer: ${x.getClass.getSimpleName}"))
          case None => Left(Interpreter.RuntimeError(token, s"BUG: missing 'this' pointer"))
        }
      )
    }

    private def convertToLox(value: Any): Either[InterpreterError, LiteralValue] = {
      value match {
        case null | () => Right(NilValue)
        case Left(RuntimeError(n, message)) => Left(Interpreter.RuntimeError(n, message))
        case Left(ExceptionalError(n, message)) => Left(Interpreter.ExceptionalError(n, message))
        case Right(l: LiteralValue) => Right(l)
        case Right(i: Instance) => Right(ClassInstanceValue(i))
        case l: LiteralValue => Right(l)
        case i: Instance => Right(ClassInstanceValue(i))
        case i: Int => Right(NumberValue(i))
        case l: Long => Right(NumberValue(l))
        case bi: BigInt => Right(NumberValue(bi.doubleValue))
        case f: Float => Right(NumberValue(f))
        case d: Double => Right(NumberValue(d))
        case bd: BigDecimal => Right(NumberValue(bd.doubleValue))
        case s: String => Right(StringValue(s))
        case b: Boolean => Right(BooleanValue(b))
        case x => Left(Interpreter.RuntimeError(token, s"BUG: invalid type for native method return: ${x.getClass.getSimpleName}"))
      }
    }

    override protected def withNewEnvironment(environment: Environment): LoxFunctionBase = {
      new NativeClassFunction(method, environment, staticInstance)
    }

    override def call(environment: Environment, arguments: Seq[LiteralValue]): Either[InterpreterError, (LiteralValue, Environment)] = {
      for {
        instance <- fetchInstance(closure)
        nativeArgs <- arguments.zip(method.getParameterTypes).map {
          case (l: LiteralValue, cls) if cls.isAssignableFrom(classOf[LiteralValue]) => Right(l)
          case (NilValue, _) => Right(null)
          case (ClassInstanceValue(i), cls) if cls.isAssignableFrom(i.getClass) => Right(i)
          case (NumberValue(n), cls) if cls.isAssignableFrom(classOf[Long]) => Right(Long.box(n.toLong))
          case (NumberValue(n), cls) if cls.isAssignableFrom(classOf[Int]) => Right(Int.box(n.toInt))
          case (NumberValue(n), cls) if cls.isAssignableFrom(classOf[Double]) => Right(Double.box(n))
          case (NumberValue(n), cls) if cls.isAssignableFrom(classOf[BigInt]) => Right(BigInt(n.toLong))
          case (NumberValue(n), cls) if cls.isAssignableFrom(classOf[Float]) => Right(Float.box(n.toFloat))
          case (NumberValue(n), cls) if cls.isAssignableFrom(classOf[BigDecimal]) => Right(BigDecimal(n))
          case (StringValue(s), cls) if cls.isAssignableFrom(classOf[String]) => Right(s)
          case (BooleanValue(b), cls) if cls.isAssignableFrom(classOf[Boolean]) => Right(Boolean.box(b))
          case (v, cls) => Left[InterpreterError, AnyRef](Interpreter.RuntimeError(token, s"Can't convert ${v.getClass.getSimpleName} to ${cls.getSimpleName}"))
        }.foldLeft[Either[InterpreterError, Seq[AnyRef]]](Right(Seq.empty)) {
          case (l @ Left(_), _) => l
          case (_, Left(err)) => Left(err)
          case (Right(values), Right(value)) => Right(values :+ value)
        }
        result <- Try(method.invoke(instance, nativeArgs: _*)) match {
          case Success(r) => Right(r)
          case Failure(t) => Left(Interpreter.RuntimeError(token, s"Failed to run native function: $t"))
        }
        returnValue <- convertToLox(result)
      } yield (returnValue, environment)
    }
  }

  class NativeClassConstructor

  class LoxNativeClass(nameToken: Token,
                       metaclass: LoxMetaClass,
                       superclass: Option[LoxClassBase],
                       methods: Map[String, LoxFunctionBase],
                       getters: Map[String, LoxFunctionBase])
    extends LoxClass(nameToken, metaclass, superclass, methods, getters)
    with LoxLogger
  {
    override def call(environment: Environment, arguments: Seq[LiteralValue]): Either[InterpreterError, (LiteralValue, Environment)] = {
      debug(nameToken, s"Calling $name")
      val args = if (name == metaclass.name) ClassInstanceValue(this) +: arguments else arguments
      super.call(environment, args)
    }
  }

  def apply(companion: NativeClass, cls: Option[Class[_ <: NativeInstance]], environment: Environment): LoxClass = {
    val nameToken = Token.dummyIdentifier(companion.name, 0)
    val companionMethods = wrapMethods(companion.getClass, companion._staticMethodNames, environment, Option(companion))
    val (initializerMethods, staticMethods) = companionMethods.partition { case (name, _) => name == "init" }
    val metaclass = new LoxMetaClass(nameToken, staticMethods)
    val methods = initializerMethods ++ cls.map(wrapMethods(_, companion._methodNames, environment, staticInstance = None)).getOrElse(Map.empty)
    val getters = cls.map(wrapMethods(_, companion._getterNames, environment, staticInstance = None)).getOrElse(Map.empty)
    new LoxNativeClass(nameToken, metaclass, superclass = None, methods, getters)
  }

  private val methodBlacklist: Seq[(String, Seq[Class[_]])] = Seq(
    ("get", Seq(classOf[Token])),
    ("set", Seq(classOf[Token], classOf[LiteralValue])),
    ("findMethod", Seq(classOf[Instance], classOf[String])),
    ("findGetter", Seq(classOf[Instance], classOf[String])),
    ("call", Seq(classOf[Environment], classOf[Seq[LiteralValue]])),
  )

  private def isInBlacklist(m: Method): Boolean = {
    methodBlacklist.exists { case (name, paramTypes) =>
      name == m.getName && m.getParameterTypes.toSeq == paramTypes
    }
  }

  private def wrapMethods(cls: Class[_], methodNames: Set[String], environment: Environment, staticInstance: Option[NativeClass]): Map[String, LoxFunctionBase] = {
    cls.getMethods
      .filter(m => methodNames.contains(m.getName))
      .filterNot(isInBlacklist)
      .foldLeft(Seq.empty[LoxFunctionBase])((ms, m) => ms :+ new NativeClassFunction(m, environment, staticInstance))
      .map(f => (f.name, f))
      .toMap
  }
}

abstract class NativeClass {
  def name: String
  protected lazy val nameToken: Token = Token.dummyIdentifier(name, 0)
  protected[native] def _staticMethodNames: Set[String]
  protected[native] def _instanceCls: Option[Class[_ <: NativeInstance]]
  protected[native] def _methodNames: Set[String]
  protected[native] def _getterNames: Set[String] = Set.empty
}
