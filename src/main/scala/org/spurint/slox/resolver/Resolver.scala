package org.spurint.slox.resolver

import org.spurint.slox.parser.{Expr, Stmt}
import org.spurint.slox.scanner.Token
import org.spurint.slox.util.LoxLogger

object Resolver extends LoxLogger {
  case class ResolverError(token: Token, message: String)

  sealed trait VariableState
  object VariableState {
    case object Declared extends VariableState
    case object Defined extends VariableState
    case object Read extends VariableState
  }

  sealed trait FunctionType
  object FunctionType {
    case object None extends FunctionType
    case object Function extends FunctionType
    case object Initializer extends FunctionType
    case object Method extends FunctionType
  }

  sealed trait ClassType
  object ClassType {
    case object None extends ClassType
    case object Class extends ClassType
  }

  sealed trait LoopType
  object LoopType {
    case object None extends LoopType
    case object Loop extends LoopType
  }

  case class State(scopes: List[Map[String, (Int, VariableState)]] = Nil,
                   locals: Map[Int, Int] = Map.empty[Int, Int],
                   functionContext: FunctionType = FunctionType.None,
                   classContext: ClassType = ClassType.None,
                   loopContext: LoopType = LoopType.None)
  {
    def beginScope(): State = {
      debug(s"Beginning scope ${scopes.length + 1}")
      copy(scopes = Map.empty[String, (Int, VariableState)] :: scopes)
    }

    def endScope(): State = {
      debug(s"Ending scope ${scopes.length}")
      scopes.head.collect {
        case (name, (line, varState)) if varState != VariableState.Read => (line, name)
      }.foreach { case (line, name) =>
        warn(Token.dummyIdentifier(name, line), s"Local variable $name is not used")
      }
      copy(scopes = scopes.tail)
    }

    def scoped(f: State => Either[ResolverError, State]): Either[ResolverError, State] = f(beginScope()).map(_.endScope())

    def declare(name: Token): State = addVar(name, VariableState.Declared)

    def define(name: Token): State = addVar(name, VariableState.Defined)

    def read(name: Token): State = {
      scopes.zipWithIndex.reverse.collectFirst {
        case (scope, idx) if scope.contains(name.lexeme) =>
          val newScope = scope(name.lexeme) match {
            case (line, VariableState.Declared) =>
              warn(name, s"Reading uninitilalized variable ${name.lexeme}")
              scope + (name.lexeme -> (line, VariableState.Read))
            case (line, VariableState.Defined) =>
              scope + (name.lexeme -> (line, VariableState.Read))
            case (_, VariableState.Read) =>
              scope
          }
          (newScope, idx)
      } match {
        case Some((newScope, idx)) =>
          debug(name, s"Setting ${name.lexeme} to read at scope level ${idx + 1}")
          copy(scopes = (scopes.take(idx) :+ newScope) ++ scopes.drop(idx + 1))
        case _ =>
          debug(name, s"Attempted to set var ${name.lexeme} to Read, but no scopes available (assuming global)")
          this
      }
    }

    private def addVar(name: Token, varState: VariableState): State = {
      scopes match {
        case Nil =>
          debug(name, s"Attempted to set var ${name.lexeme} to $varState, but no scopes are available (assuming global)")
          this
        case head :: tail =>
          debug(name, s"Setting ${name.lexeme} to $varState at scope level ${scopes.length}")
          val declaredLine = head.get(name.lexeme).map { case (line, _) => line }.getOrElse(name.line)
          copy(scopes = (head + (name.lexeme -> (declaredLine, varState))) :: tail)
      }
    }

    def functionBody(functionType: FunctionType)(f: State => Either[ResolverError, State]): Either[ResolverError, State] = {
      val outerFunctionType = functionContext
      f(copy(functionContext = functionType)).map(_.copy(functionContext = outerFunctionType))
    }

    def classBody(classType: ClassType)(f: State => Either[ResolverError, State]): Either[ResolverError, State] = {
      val outerClassType = classContext
      f(copy(classContext = classType)).map(_.copy(classContext = outerClassType))
    }

    def loopBody(loopType: LoopType)(f: State => Either[ResolverError, State]): Either[ResolverError, State] = {
      val outerLoopType = loopContext
      f(copy(loopContext = loopType)).map(_.copy(loopContext = outerLoopType))
    }

    def resolve(expr: Expr, depth: Int): State = copy(locals = locals + (System.identityHashCode(expr) -> depth))
  }

  def apply(statements: Seq[Stmt], initialResolvedLocals: Map[Int, Int] = Map.empty[Int, Int]): Either[ResolverError, Map[Int, Int]] = {
    resolve(State(locals = initialResolvedLocals), statements).map(_.locals)
  }

  private def resolve(state: State, statements: Seq[Stmt]): Either[ResolverError, State] = {
    statements.foldLeft[Either[ResolverError, State]](Right(state))(
      (state, stmt) => state.flatMap(resolve(_, stmt))
    )
  }

  private def resolve(state: State, stmt: Stmt): Either[ResolverError, State] = {
    stmt match {
      case b: Stmt.Block => resolveBlockStmt(state, b)
      case b: Stmt.Break => resolveBreakStmt(state, b)
      case c: Stmt.Class => resolveClassStmt(state, c)
      case c: Stmt.Continue => resolveContinueStmt(state, c)
      case e: Stmt.Expression => resolveExpressionStmt(state, e)
      case f: Stmt.For => resolveForStmt(state, f)
      case f: Stmt.Function => resolveFunctionStmt(state, f)
      case f: Stmt.If => resolveIfStmt(state, f)
      case p: Stmt.Print => resolvePrintStmt(state, p)
      case r: Stmt.Return => resolveReturnStmt(state, r)
      case v: Stmt.Var => resolveVarStmt(state, v)
    }
  }

  private def resolveBlockStmt(state: State, stmt: Stmt.Block): Either[ResolverError, State] = {
    debug(stmt, s"Entering block ")
    val result = state.scoped(resolve(_, stmt.statements))
    debug(stmt, s"Leaving block")
    result
  }

  private def resolveBreakStmt(state: State, stmt: Stmt.Break): Either[ResolverError, State] = {
    if (state.loopContext == LoopType.None) {
      Left(ResolverError(stmt.keyword, "Cannot break outside a loop"))
    } else {
      Right(state)
    }
  }

  private def resolveClassStmt(state: State, stmt: Stmt.Class): Either[ResolverError, State] = {
    val nameState = state.declare(stmt.name).define(stmt.name)
    nameState.classBody(ClassType.Class) { classState =>
      classState.scoped { innerState =>
        val thisState = innerState.define(Token.thisToken(stmt.line))
        for {
          superclassState <- stmt.superclass.map(resolve(thisState, _)).getOrElse(Right(thisState))
          staticMethodsState <- stmt.staticMethods.foldLeft[Either[ResolverError, State]](Right(superclassState))(
            (state, method) => state.flatMap(resolveFunction(_, method.function, FunctionType.Method))
          )
          methodsState <- stmt.methods.foldLeft[Either[ResolverError, State]](Right(staticMethodsState)) { (state, method) =>
            val functionType =
              if (method.name.lexeme == "init") FunctionType.Initializer
              else FunctionType.Method
            state.flatMap(resolveFunction(_, method.function, functionType))
          }
          finalState <- stmt.getters.foldLeft[Either[ResolverError, State]](Right(methodsState))(
            (state, getter) => state.flatMap(resolveFunction(_, getter.function, FunctionType.Method))
          )
        } yield finalState
      }
    }
  }

  private def resolveContinueStmt(state: State, stmt: Stmt.Continue): Either[ResolverError, State] = {
    if (state.loopContext == LoopType.None) {
      Left(ResolverError(stmt.keyword, "Cannot continue outside a loop"))
    } else {
      Right(state)
    }
  }

  private def resolveExpressionStmt(state: State, stmt: Stmt.Expression): Either[ResolverError, State] = {
    resolve(state, stmt.expression)
  }

  private def resolveForStmt(state: State, stmt: Stmt.For): Either[ResolverError, State] = {
    for {
      state1 <- stmt.initializer.map(resolve(state, _)).getOrElse(Right(state))
      state2 <- resolve(state1, stmt.condition)
      state3 <- stmt.increment.map(resolve(state2, _)).getOrElse(Right(state2))
      state5 <- state3.loopBody(LoopType.Loop) { state4 =>
        resolve(state4, stmt.body)
      }
    } yield state5
  }

  private def resolveFunctionStmt(state: Resolver.State, stmt: Stmt.Function): Either[ResolverError, State] = {
    val nameState = state.declare(stmt.name).define(stmt.name)
    resolveFunction(nameState, stmt.function, FunctionType.Function)
  }

  private def resolveIfStmt(state: State, stmt: Stmt.If): Either[ResolverError, State] = {
    resolve(state, stmt.condition)
      .flatMap(resolve(_, stmt.thenBranch))
      .flatMap(s => stmt.elseBranch.map(resolve(s, _)).getOrElse(Right(s)))
  }

  private def resolvePrintStmt(state: State, stmt: Stmt.Print): Either[ResolverError, State] = {
    resolve(state, stmt.expression)
  }

  private def resolveReturnStmt(state: State, stmt: Stmt.Return): Either[ResolverError, State] = {
    state.functionContext match {
      case FunctionType.None => Left(ResolverError(stmt.keyword, "Cannot return from top-level code."))
      case FunctionType.Initializer => Left(ResolverError(stmt.keyword, "Cannot return a value from an initializer."))
      case FunctionType.Function | FunctionType.Method => resolve(state, stmt.value)
    }
  }

  private def resolveVarStmt(state: State, stmt: Stmt.Var): Either[ResolverError, State] = {
    val declaredState = state.declare(stmt.name)
    stmt.initializer.map(resolve(declaredState, _)).getOrElse(Right(declaredState)).map { initializerState =>
      initializerState.define(stmt.name)
    }
  }

  private def resolve(state: State, expr: Expr): Either[ResolverError, State] = {
    expr match {
      case a: Expr.Assign => resolveAssignExpr(state, a)
      case b: Expr.Binary => resolveBinaryExpr(state, b)
      case c: Expr.Call => resolveCallExpr(state, c)
      case f: Expr.Function => resolveFunctionExpr(state, f)
      case g: Expr.Get => resolveGetExpr(state, g)
      case g: Expr.Grouping => resolveGroupingExpr(state, g)
      case l: Expr.Literal => resolveLiteralExpr(state, l)
      case l: Expr.Logical => resolveLogicalExpr(state, l)
      case s: Expr.Set => resolveSetExpr(state, s)
      case t: Expr.This => resolveThisExpr(state, t)
      case u: Expr.Unary => resolveUnaryExpr(state, u)
      case v: Expr.Variable => resolveVariableExpr(state, v)
    }
  }

  private def resolveLocal(state: State, expr: Expr, name: Token, isRead: Boolean): Either[ResolverError, State] = {
    Right(state.scopes.reverse.zipWithIndex.reverse.collectFirst {
      case (scope, idx) if scope.contains(name.lexeme) =>
        debug(name, s"Found local var ${name.lexeme} in scope ${idx + 1}")
        idx
    }.map({ idx =>
      val depth = state.scopes.length - 1 - idx
      debug(name, s"Marking ${name.lexeme} resolved at depth $depth for ${expr.getClass.getSimpleName} ${System.identityHashCode(expr)}")
      state.resolve(expr, depth).read(name)
    }).getOrElse {
      debug(name, s"Failed to find var ${name.lexeme}; assuming it's global (scope depth is ${state.scopes.length})")
      state
    })
  }

  private def resolveFunction(state: State, expr: Expr.Function, functionType: FunctionType): Either[ResolverError, State] = {
    state.functionBody(functionType) { functionContextState =>
      functionContextState.loopBody(LoopType.None) { noLoopState =>
        noLoopState.scoped { functionState =>
          val parametersState = expr.parameters.foldLeft(functionState)(
            (state, token) => state.declare(token).define(token)
          )
          resolve(parametersState, expr.body)
        }
      }
    }
  }

  private def resolveAssignExpr(state: State, expr: Expr.Assign): Either[ResolverError, State] = {
    resolve(state, expr.value).flatMap(resolveLocal(_, expr, expr.name, isRead = false))
  }

  private def resolveBinaryExpr(state: State, expr: Expr.Binary): Either[ResolverError, State] = {
    resolve(state, expr.left).flatMap(resolve(_, expr.right))
  }

  private def resolveCallExpr(state: State, expr: Expr.Call): Either[ResolverError, State] = {
    resolve(state, expr.callee).flatMap(state =>
      expr.arguments.foldLeft[Either[ResolverError, State]](Right(state))(
        (state, arg) => state.flatMap(resolve(_, arg))
      )
    )
  }

  private def resolveFunctionExpr(state: State, expr: Expr.Function): Either[ResolverError, State] = {
    resolveFunction(state, expr, FunctionType.Function)
  }

  private def resolveGetExpr(state: State, expr: Expr.Get): Either[ResolverError, State] = {
    resolve(state, expr.obj)
  }

  private def resolveGroupingExpr(state: State, expr: Expr.Grouping): Either[ResolverError, State] = {
    resolve(state, expr.expression)
  }

  private def resolveLiteralExpr(state: State, expr: Expr.Literal): Either[ResolverError, State] = {
    Right(state)
  }

  private def resolveLogicalExpr(state: State, expr: Expr.Logical): Either[ResolverError, State] = {
    resolve(state, expr.left).flatMap(resolve(_, expr.right))
  }

  private def resolveSetExpr(state: State, expr: Expr.Set): Either[ResolverError, State] = {
    resolve(state, expr.value).flatMap(resolve(_, expr.obj))
  }

  private def resolveThisExpr(state: State, expr: Expr.This): Either[ResolverError, State] = {
    if (state.classContext != ClassType.Class) {
      Left(ResolverError(expr.keyword, "Cannot use 'this' outside of a class."))
    } else {
      resolveLocal(state, expr, expr.keyword, isRead = true)
    }
  }

  private def resolveUnaryExpr(state: State, stmt: Expr.Unary): Either[ResolverError, State] = {
    resolve(state, stmt.right)
  }

  private def resolveVariableExpr(state: State, expr: Expr.Variable): Either[ResolverError, State] = {
    state.scopes match {
      case head :: _ if head.get(expr.name.lexeme).exists { case (_, varState) => varState == VariableState.Declared } =>
        Left(ResolverError(expr.name, "Cannot read local variable in its own initializer."))
      case _ =>
        resolveLocal(state, expr, expr.name, isRead = true)
    }
  }
}
