package org.spurint.slox.interpreter

import org.spurint.slox.interpreter.Environment.{ScopeError, UndefinedVariableError}
import org.spurint.slox.model.LiteralValue
import org.spurint.slox.scanner.Token
import org.spurint.slox.util._
import scala.annotation.tailrec

object Environment {
  case class ScopeError(id: Option[String] = None)
  case class UndefinedVariableError(variable: Token)

  val global: Environment = {
    val env = Environment("GLOBAL")
    native.registerNativeFunctions(env)
  }

  def apply(id: String): Environment = new Environment(id, Map.empty[String, LiteralValue[_]], enclosing = None)
}

class Environment private (val id: String, private val values: Map[String, LiteralValue[_]], private val enclosing: Option[Environment]) extends LoxLogger with HasIdentifier {
  override protected def debug(line: HasLineInfo, msg: => Any): Unit = {
    super.debug(line, s"[$id] $msg")
  }

  override protected def debug(msg: => Any): Unit = {
    super.debug(s"[$id] $msg")
  }

  def define(name: HasLineInfo with HasIdentifier, value: LiteralValue[_]): Environment = {
    debug(name, s"Defining ${name.id}=$value (our depth is $depth)")
    new Environment(id, values + (name.id -> value), enclosing)
  }

  private def assignHere(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    if (values.contains(name.lexeme)) {
      Right(new Environment(id, values + (name.lexeme -> value), enclosing))
    } else {
      Left(UndefinedVariableError(name))
    }
  }

  def assignAt(distance: Int, name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    debug(name, s"Assign ${name.lexeme}=$value at dist $distance (our depth is $depth)")
    ancestor(distance).map(
      assignEnv => assignEnv.assignHere(name, value).map(fixAssignChain(assignEnv, _))
    ).getOrElse(Left(UndefinedVariableError(name)))
  }

  def assignAtRoot(name: Token, value: LiteralValue[_]): Either[UndefinedVariableError, Environment] = {
    debug(name, s"Assign ${name.lexeme}=$value at root (our depth is $depth)")
    root.assignHere(name, value).map(fixAssignChain(root, _))
  }

  private def fixAssignChain(assignEnv: Environment, newTail: Environment): Environment = {
    if (assignEnv == this) {
      newTail
    } else {
      val newEnvironments = collectAll().takeWhile(_ != assignEnv) ++ newTail.collectAll()
      buildEnvironmentFromList(newEnvironments)
    }
  }

  private def get(name: Token): Option[LiteralValue[_]] = {
    debug(name, s"Get ${name.lexeme} recursively (our depth is $depth, id=$id, enc=${enclosing.map(_.id)})")
    getHere(name).orElse(enclosing.flatMap(enc => enc.get(name)))
  }

  private def getHere(name: Token): Option[LiteralValue[_]] = {
    values.get(name.lexeme)
  }

  def getAt(distance: Int, name: Token): Option[LiteralValue[_]] = {
    debug(name, s"Get ${name.lexeme} from dist $distance (our depth is $depth)")
    val result = ancestor(distance).flatMap(_.getHere(name))
    if (logger.isDebugEnabled()) {
      result.getOrElse {
        debug(name, s"Failed to find ${name.lexeme}; searching for the real deal")
        get(name) match {
          case Some(v) => debug(name, s"Found ${name.lexeme} ($v) at wrong depth")
          case _ => debug(name, s"Couldn't find ${name.lexeme} at all")
        }
        debug(name, s"Environments (here to bottom) are:")
        def rec(env: Environment): Unit = {
          debug(name, s"    ${env.id}: ${env.values}")
          env.enclosing.foreach(rec)
        }
        rec(this)
      }
    }
    result
  }

  def getAtRoot(name: Token): Option[LiteralValue[_]] = {
    debug(name, s"Get ${name.lexeme} from root (our depth is $depth)")
    root.getHere(name)
  }

  def pushScope(id: String): Environment = {
    debug(s"Pushing scope; new depth will be ${depth + 1}")
    new Environment(id, Map.empty[String, LiteralValue[_]], Some(this))
  }

  def popScopeTo(id: String): Either[ScopeError, Environment] = {
    if (this.id ==  id) {
      debug(s"Popping scope to depth $depth")
      Right(this)
    } else {
      enclosing.map(_.popScopeTo(id)).getOrElse(Left(ScopeError(Option(id))))
    }
  }

  def mergeIn(environment: Environment): Environment = {
    val mergedEnvironments = collectAll().map { baseEnvironment =>
      environment.findWithId(baseEnvironment.id) match {
        case Some(mergeEnvironment) =>
          new Environment(baseEnvironment.id, baseEnvironment.values ++ mergeEnvironment.values, None)
        case _ =>
          baseEnvironment
      }
    }
    buildEnvironmentFromList(mergedEnvironments)
  }

  private def buildEnvironmentFromList(environments: Seq[Environment]): Environment = {
    environments.reduceRight(
      (env, lower) => new Environment(env.id, env.values, Option(lower))
    )
  }

  private def collectAll(): Seq[Environment] = {
    def rec(curEnvironment: Environment): List[Environment] = {
      curEnvironment.enclosing match {
        case Some(enc) => curEnvironment :: rec(enc)
        case _ => curEnvironment :: Nil
      }
    }
    rec(this)
  }

  private def ancestor(distance: Int): Option[Environment] = {
    0.until(distance).foldLeft(Option(this))(
      (lastEnv, _) => lastEnv.flatMap(_.enclosing)
    )
  }

  private def findWithId(otherId: String): Option[Environment] = {
    @tailrec
    def rec(curEnvironment: Environment): Option[Environment] = {
      if (curEnvironment.id == otherId) {
        Some(curEnvironment)
      } else {
        curEnvironment.enclosing match {
          case Some(enc) => rec(enc)
          case _ => None
        }
      }
    }
    rec(this)
  }

  private lazy val (root: Environment, depth: Int) = {
    @tailrec
    def rec(environment: Environment, depth: Int): (Environment, Int) = {
      environment.enclosing match {
        case Some(enc) => rec(enc, depth + 1)
        case _ => (environment, depth)
      }
    }
    rec(this, 0)
  }
}
