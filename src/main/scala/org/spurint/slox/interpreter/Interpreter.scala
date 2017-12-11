package org.spurint.slox.interpreter

import java.util.UUID
import org.spurint.slox.model.LiteralValue._
import org.spurint.slox.model.{LiteralValue, LoxCallable}
import org.spurint.slox.parser.{Expr, Stmt}
import org.spurint.slox.scanner.Token
import org.spurint.slox.util._
import scala.annotation.tailrec

object Interpreter extends LoxLogger {
  sealed trait InterpreterError
  case class RuntimeError(token: Token, message: String) extends InterpreterError

  sealed trait ControlFlowChange extends InterpreterError
  case class Return(value: LiteralValue, environment: Environment) extends ControlFlowChange
  case class Break(state: State) extends ControlFlowChange
  case class Continue(state: State) extends ControlFlowChange

  case class State(environment: Environment, resolvedLocals: Map[Int, Int]) {
    def lookUpVariable(name: Token, expr: Expr): Option[LiteralValue] = {
      resolvedLocals.get(System.identityHashCode(expr))
        .map(environment.getAt(_, name))
        .getOrElse(environment.getAtRoot(name))
    }

    def defineVariable(name: Token, value: LiteralValue): State = copy(environment = environment.define(name, value))

    def assignVariable(name: Token, expr: Expr, value: LiteralValue): Either[InterpreterError, State] = {
      resolvedLocals.get(System.identityHashCode(expr))
        .map(distance => environment.assignAt(distance, name, value))
        .getOrElse(environment.assignAtRoot(name, value))
        .leftMap(_ => RuntimeError(name, "Attempt to assign to an undefined variable"))
        .map(newEnv => copy(environment = newEnv))
    }

    def assignVariable(name: Token, value: LiteralValue): Either[InterpreterError, State] = {
      environment.assign(name, value)
        .leftMap(_ => RuntimeError(name, "Attempt to assign to an undefined variable"))
        .map(env => copy(environment = env))
    }

    def pushScope(id: String): State = copy(environment = environment.pushScope(id))

    def popScopeTo(id: String): Either[InterpreterError, State] = {
      environment.popScopeTo(id).leftMap(interpreterScopeError).map(env => copy(environment = env))
    }
  }

  def apply(stmts: Seq[Stmt],
            initialEnvironment: Option[Environment] = None,
            resolvedLocals: Map[Int, Int] = Map.empty[Int, Int]): Either[InterpreterError, Environment] =
  {
    apply(stmts, State(initialEnvironment.getOrElse(Environment.global), resolvedLocals))
  }

  def apply(stmts: Seq[Stmt],
            initialEnvironment: Environment,
            resolvedLocals: Map[Int, Int]): Either[InterpreterError, Environment] =
  {
    apply(stmts, Option(initialEnvironment), resolvedLocals)
  }

  private def apply(stmts: Seq[Stmt], initialState: State): Either[InterpreterError, Environment] = {
    @tailrec
    def rec(stmts: Seq[Stmt], state: State): Either[InterpreterError, State] = {
      stmts match {
        case lastStmt :: Nil => execute(lastStmt, state)
        case nextStmt :: moreStmts => execute(nextStmt, state) match {
          case Right(state1) => rec(moreStmts, state1)
          case l => l
        }
        case Nil => Right(state)
      }
    }
    rec(stmts, initialState).map(_.environment)
  }

  private def execute(stmt: Stmt, state: State): Either[InterpreterError, State] = {
    stmt match {
      case b: Stmt.Block => executeBlockStmt(b, state)
      case b: Stmt.Break => executeBreakStmt(b, state)
      case c: Stmt.Class => executeClassStmt(c, state)
      case c: Stmt.Continue => executeContinueStmt(c, state)
      case e: Stmt.Expression => executeExpressionStmt(e, state)
      case f: Stmt.For => executeForStmt(f, state)
      case f: Stmt.Function => executeFunctionStmt(f, state)
      case i: Stmt.If => executeIfStmt(i, state)
      case p: Stmt.Print => executePrintStmt(p, state)
      case r: Stmt.Return => executeReturnStmt(r, state)
      case v: Stmt.Var => executeVarStmt(v, state)
    }
  }

  private def executeBlockStmt(block: Stmt.Block, state: State): Either[InterpreterError, State] = {
    val oldScopeId = state.environment.id
    val newScopeId = s"block-${block.hashCode}-${UUID.randomUUID()}"
    debug(block, s"Executing block $newScopeId")

    def popScope(innerState: State): Either[InterpreterError, State] = {
      innerState.popScopeTo(oldScopeId).recoverWith { case _ => Right(innerState) }
    }
    def recoverLoopControl(innerState: State, newError: State => InterpreterError): InterpreterError = {
      popScope(innerState) match {
        case Right(outerState) => newError(outerState)
        case Left(err) => err
      }
    }

    block.statements.foldLeft[Either[InterpreterError, State]](Right(state.pushScope(newScopeId))) {
      case (Right(curState), stmt) => execute(stmt, curState)
      case (l, _) => l
    } match {
      case Right(state1) => popScope(state1)
      case Left(Interpreter.Break(state1)) => Left(recoverLoopControl(state1, Interpreter.Break.apply))
      case Left(Interpreter.Continue(state1)) => Left(recoverLoopControl(state1, Interpreter.Continue.apply))
      case x => x
    }
  }

  private def executeBreakStmt(break: Stmt.Break, state: State): Either[InterpreterError, State] = {
    debug(break.keyword, "gonna break!")
    Left(Interpreter.Break(state))
  }

  private def executeClassStmt(stmt: Stmt.Class, state: State): Either[InterpreterError, State] = {
    val definedState = state.defineVariable(stmt.name, NilValue)
    val superclassResult = stmt.superclass match {
      case Some(superclass) =>
        evaluate(superclass, definedState) match {
          case Right((CallableValue(cls: LoxClass), superclassState)) => Right(Option(cls), superclassState)
          case Right(_) => Left(RuntimeError(stmt.name, "Superclass must be a class."))
          case l @ Left(_) => l.rightCast
        }
      case None => Right((None, definedState))
    }
    superclassResult.flatMap { case (superclass, superclassState) =>
      val staticMethods = stmt.staticMethods.map(method =>
        method.name.lexeme -> new LoxFunction(Option(method.name), method.function, superclassState.environment, superclassState.resolvedLocals, isInitializer = false)
      ).toMap
      val metaclass = new LoxMetaClass(stmt.name, staticMethods)
      val methods = stmt.methods.map { method =>
        val isInitializer = method.name.lexeme == "init"
        method.name.lexeme -> new LoxFunction(Option(method.name), method.function, superclassState.environment, superclassState.resolvedLocals, isInitializer)
      }.toMap
      val getters = stmt.getters.map { getter =>
        getter.name.lexeme -> new LoxFunction(Option(getter.name), getter.function, superclassState.environment, superclassState.resolvedLocals, isInitializer = false)
      }.toMap
      debug(stmt, s"Creating class ${stmt.name.lexeme} with superclass ${superclass.map(_.name).getOrElse("(none)")} with methods ${methods.keys}")
      val cls = new LoxClass(stmt.name, metaclass, superclass, methods, getters)
      superclassState.assignVariable(stmt.name, CallableValue(cls))
    }
  }

  private def executeContinueStmt(continue: Stmt.Continue, state: State): Either[InterpreterError, State] = {
    debug(continue.keyword, "gonna continue!")
    Left(Interpreter.Continue(state))
  }

  private def executeExpressionStmt(stmt: Stmt.Expression, state: State): Either[InterpreterError, State] = {
    evaluate(stmt.expression, state).map { case (_, state1) => state1 }
  }

  private def executeFunctionStmt(stmt: Stmt.Function, state: State): Either[InterpreterError, State] = {
    val function = new LoxFunction(Option(stmt.name), stmt.function, state.environment, state.resolvedLocals, isInitializer = false)
    Right(state.defineVariable(stmt.name, CallableValue(function)))
  }

  private def executeIfStmt(stmt: Stmt.If, state: State): Either[InterpreterError, State] = {
    evaluate(stmt.condition, state).flatMap { case (conditionResult, state1) =>
      if (isTruthy(conditionResult)) {
        execute(stmt.thenBranch, state1)
      } else {
        stmt.elseBranch.map(execute(_, state1)).getOrElse(Right(state1))
      }
    }
  }

  private def executePrintStmt(stmt: Stmt.Print, state: State): Either[InterpreterError, State] = {
    for {
      res <- evaluate(stmt.expression, state)
      (value, state1) = res
    } yield {
      println(value.toString)
      state1
    }
  }

  private def executeReturnStmt(stmt: Stmt.Return, state: State): Either[InterpreterError, State] = {
    evaluate(stmt.value, state).flatMap { case (expr, state1) => Left(Return(expr, state1.environment)) }
  }

  private def executeVarStmt(stmt: Stmt.Var, state: State): Either[InterpreterError, State] = {
    for {
      res <- stmt.initializer.map(evaluate(_, state)).getOrElse(Right(NilValue -> state))
      (value, state1) = res
    } yield {
      state1.defineVariable(stmt.name, value)
    }
  }

  private def executeForStmt(stmt: Stmt.For, state: State): Either[InterpreterError, State] = {
    @tailrec
    def rec(state: State): Either[InterpreterError, State] = {
      @inline
      def doIncrement(state: State): Either[InterpreterError, State] = {
        stmt.increment.map(execute(_, state)).getOrElse(Right(state))
      }

      evaluate(stmt.condition, state) match {
        case Right((conditionResult, state1)) =>
          if (isTruthy(conditionResult)) {
            execute(stmt.body, state1) match {
              case Right(state2) => doIncrement(state2) match {
                case Right(state3) => rec(state3)
                case l => l
              }
              case Left(Interpreter.Continue(state2)) => doIncrement(state2) match {
                case Right(state3) => rec(state3)
                case l => l
              }
              case l => l
            }
          } else {
            Right(state1)
          }
        case l @ Left(_) => l.rightCast
      }
    }
    stmt.initializer.map(execute(_, state)).getOrElse(Right(state)).flatMap(rec).recover {
      case Interpreter.Break(state1) => state1
    }
  }

  private def evaluate(expr: Expr, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    expr match {
      case l: Expr.Literal => evaluateLiteral(l, state)
      case u: Expr.Unary => evaluateUnary(u, state)
      case b: Expr.Binary => evaluateBinary(b, state)
      case f: Expr.Function => evaluateFunction(f, state)
      case g: Expr.Grouping => evaluateGrouping(g, state)
      case v: Expr.Variable => evaluateVariable(v, state).map(_ -> state)
      case a: Expr.Assign => evaluateAssign(a, state)
      case l: Expr.Logical => evaluateLogical(l, state)
      case c: Expr.Call => evaluateCall(c, state)
      case g: Expr.Get => evaluateGet(g, state)
      case s: Expr.Set => evaluateSet(s, state)
      case t: Expr.This => evaluateThis(t, state)
    }
  }

  private def evaluateLiteral(literal: Expr.Literal, state: State): Either[InterpreterError, (LiteralValue, State)] =
    Right(literal.value -> state)

  private def evaluateUnary(unary: Expr.Unary, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    evaluate(unary.right, state).flatMap { case (right, state1) =>
      unary.operator.`type` match {
        case Token.Type.Bang => Right(BooleanValue(!isTruthy(right)) -> state1)
        case Token.Type.Minus => right match {
          case NumberValue(n) => Right(NumberValue(-n) -> state1)
          case _ => Left(RuntimeError(unary.operator, "Cannot negate a non-numeric value"))
        }
        case _ => Left(RuntimeError(unary.operator, "Cannot perform unary operation with this operator"))
      }
    }
  }

  private def isTruthy(value: LiteralValue): Boolean = {
    value match {
      case NilValue => false
      case BooleanValue(b) => b
      case _ => true
    }
  }

  private def evaluateBinary(binary: Expr.Binary, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    for {
      res0 <- evaluate(binary.left, state)
      (left, state1) = res0
      res1 <- evaluate(binary.right, state1)
      (right, state2) = res1
      value <- evaluateBinary(binary, left, binary.operator, right).map(_ -> state2)
    } yield value
  }

  private def evaluateBinary(binary: Expr.Binary, left: LiteralValue, operator: Token, right: LiteralValue): Either[InterpreterError, LiteralValue] = {
    operator.`type` match {
      case Token.Type.EqualEqual => Right(BooleanValue(isEqual(left, right)))
      case Token.Type.BangEqual => Right(BooleanValue(!isEqual(left, right)))
      case Token.Type.LessEqual => compare(binary, left, right).map(cmp => BooleanValue(cmp <= 0))
      case Token.Type.Less => compare(binary, left, right).map(cmp => BooleanValue(cmp < 0))
      case Token.Type.GreaterEqual => compare(binary, left, right).map(cmp => BooleanValue(cmp >= 0))
      case Token.Type.Greater => compare(binary, left, right).map(cmp => BooleanValue(cmp > 0))
      case Token.Type.Star | Token.Type.Slash | Token.Type.Plus | Token.Type.Minus =>
        arithmetic(binary, left, operator, right)
      case Token.Type.And | Token.Type.Or => compareBoolean(binary, left, right).map(BooleanValue.apply)
      case _ => Left(RuntimeError(operator, "Cannot evaluate binary expression joined by this operator"))
    }
  }

  private def arithmetic(binary: Expr.Binary, left: LiteralValue, operator: Token, right: LiteralValue): Either[InterpreterError, LiteralValue] = {
    (left, right) match {
      case (NumberValue(ln), NumberValue(rn)) => operator.`type` match {
        case Token.Type.Star => Right(NumberValue(ln * rn))
        case Token.Type.Slash =>
          if (rn == 0) Left(RuntimeError(operator, "Division by zero"))
          else Right(NumberValue(ln / rn))
        case Token.Type.Plus => Right(NumberValue(ln + rn))
        case Token.Type.Minus => Right(NumberValue(ln - rn))
        case _ => Left(RuntimeError(operator, "Cannot perform arithmetic with this operator"))
      }
      case (lhs @ StringValue(_), rhs) => operator.`type` match {
        case Token.Type.Plus => Right(StringValue(lhs.toString + rhs.toString))
        case _ => Left(RuntimeError(operator, "Cannot perform arithmetic on string values"))
      }
      case (lhs, rhs @ StringValue(_)) => operator.`type` match {
        case Token.Type.Plus => Right(StringValue(lhs.toString + rhs.toString))
        case _ => Left(RuntimeError(operator, "Cannot perform arithmetic on string values"))
      }
      case _ => Left(RuntimeError(operator, "Cannot perform arithmetic on non-numeric values"))
    }
  }

  private def compare(binary: Expr.Binary, left: LiteralValue, right: LiteralValue): Either[InterpreterError, Int] = {
    (left, right) match {
      case (ln: NumberValue, rn: NumberValue) => Right(ln.value.compare(rn.value))
      case _ => Left(RuntimeError(binary.operator, "Cannot compare non-numeric values"))
    }
  }

  private def compareBoolean(binary: Expr.Binary, left: LiteralValue, right: LiteralValue): Either[InterpreterError, Boolean] = {
    (left, right) match {
      case (BooleanValue(lb), BooleanValue(rb)) => binary.operator.`type` match {
        case Token.Type.And => Right(lb && rb)
        case Token.Type.Or => Right(lb || rb)
        case _ => Left(RuntimeError(binary.operator, "Cannot do boolean comparison with this operator"))
      }
      case _ => Left(RuntimeError(binary.operator, "Cannot perform comparison on non-boolean values"))
    }
  }

  private def isEqual(left: LiteralValue, right: LiteralValue): Boolean = {
    (left, right) match {
      case (NilValue, NilValue) => true
      case (NilValue, _) => false
      case _ => left.equals(right)
    }
  }

  private def evaluateGet(get: Expr.Get, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    evaluate(get.obj, state).flatMap {
      case (ClassInstanceValue(instance), state1) => Right((instance, state1))
      case (CallableValue(cls: LoxClass), state1) => Right((cls, state1))
      case _ => Left(RuntimeError(get.name, "Only classes and instances have properties."))
    }.flatMap { case (instance, state1) =>
      instance.get(get.name) match {
        case Right(GettableValue(function)) => evaluateCallable(get.name, function, Seq.empty, state1)
        case Right(value: LiteralValue) => Right((value, state1))
        case l @ Left(_) => l.rightCast
      }
    }
  }

  private def evaluateSet(set: Expr.Set, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    evaluate(set.obj, state).flatMap {
      case (ClassInstanceValue(instance), state1) => Right((instance, state1))
      case (CallableValue(cls: LoxClass), state1) => Right((cls, state1))
      case _ => Left(RuntimeError(set.name, "Only classes and instances have properties."))
    }.flatMap { case (instance, state1) =>
      evaluate(set.value, state1).flatMap {
        case (value, state2) => instance.set(set.name, value).map(value => (value, state2))
      }
    }
  }

  private def evaluateThis(thisExpr: Expr.This, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    state.lookUpVariable(thisExpr.keyword, thisExpr)
      .map(value => Right((value, state)))
      .getOrElse(Left(RuntimeError(thisExpr.keyword, s"Undefined variable")))
  }

  private def evaluateGrouping(grouping: Expr.Grouping, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    evaluate(grouping.expression, state)
  }

  private def evaluateVariable(variable: Expr.Variable, state: State): Either[InterpreterError, LiteralValue] = {
    state
      .lookUpVariable(variable.name, variable)
      .map(Right.apply)
      .getOrElse(Left(RuntimeError(variable.name, s"Undefined variable")))
  }

  private def evaluateAssign(assign: Expr.Assign, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    for {
      res <- evaluate(assign.value, state)
      (value, state1) = res
      state2 <- state1.assignVariable(assign.name, assign, value).leftMap { _ =>
        RuntimeError(assign.name, "Attempt to assign to an undefined variable")
      }
    } yield {
      value -> state2
    }
  }

  private def evaluateLogical(logical: Expr.Logical, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    evaluate(logical.left, state).flatMap { case (left, state1) =>
      logical.operator.`type` match {
        case Token.Type.Or if isTruthy(left) => Right(left -> state1)
        case Token.Type.And if !isTruthy(left) => Right(left -> state1)
        case _ => evaluate(logical.right, state1)
      }
    }
  }

  private def evaluateFunction(function: Expr.Function, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    val f = new LoxFunction(fname = None, function, state.environment, state.resolvedLocals, isInitializer = false)
    Right((CallableValue(f), state))
  }

  private def evaluateCall(call: Expr.Call, state: State): Either[InterpreterError, (LiteralValue, State)] = {
    debug(call, s"Calling function ${call.callee}")
    evaluate(call.callee, state).flatMap {
      case (CallableValue(callee), state1) => evaluateCallable(call.paren, callee, call.arguments, state1)
      case (badCallee, _) => Left(RuntimeError(call.paren, s"Function callee $badCallee is not callable"))
    }
  }

  private def evaluateCallable(location: Token, callable: LoxCallable, arguments: Seq[Expr], state: State): Either[InterpreterError, (LiteralValue, State)] = {
    val initialValue: Either[InterpreterError, (Seq[LiteralValue], State)] = Right(Seq.empty -> state)
    arguments.foldLeft(initialValue) {
      case (Right((argValues, curState)), arg) => evaluate(arg, curState).map {
        case (argValue, curState1) =>
          (argValues :+ argValue) -> curState1
      }
      case (l, _) => l
    }.flatMap { case (args, argState) =>
      if (args.length != callable.arity) {
        Left(RuntimeError(location, s"Function takes ${callable.arity} arguments but ${args.length} provided"))
      } else {
        callable.call(argState.environment, args)
          .map { case (returnValue, returnEnv) => returnValue -> argState.copy(environment = returnEnv) }
      }
    }
  }
}
