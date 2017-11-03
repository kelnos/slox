package org.spurint.slox.resolver

import org.spurint.slox.parser.{Expr, Stmt}
import org.spurint.slox.scanner.Token
import org.spurint.slox.util.LoxLogger

object Resolver extends LoxLogger {
  case class ResolverError(token: Token, message: String)

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

  case class State(scopes: List[Map[String, Boolean]] = Nil,
                   locals: Map[Int, Int] = Map.empty[Int, Int],
                   functionContext: FunctionType = FunctionType.None,
                   classContext: ClassType = ClassType.None)
  {
    def beginScope(): State = {
      debug(s"Beginning scope ${scopes.length + 1}")
      copy(scopes = Map.empty[String, Boolean] :: scopes)
    }

    def endScope(): State = {
      debug(s"Ending scope ${scopes.length}")
      copy(scopes = scopes.tail)
    }

    def scoped(f: State => Either[ResolverError, State]): Either[ResolverError, State] = f(beginScope()).map(_.endScope())

    def declare(name: Token): State = addVar(name, ready = false)

    def define(name: Token): State = addVar(name, ready = true)

    private def addVar(name: Token, ready: Boolean): State = {
      scopes match {
        case Nil =>
          debug(name, s"Attempt to ${if (!ready) "declare" else "define"} var ${name.lexeme}, but no scopes are available (assuming global)")
          this
        case head :: tail =>
          debug(name, s"${if (!ready) "Declaring" else "Defining"} ${name.lexeme} at scope level ${scopes.length}")
          copy(scopes = (head + (name.lexeme -> ready)) :: tail)
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
      case f: Stmt.Function => resolveFunctionStmt(state, f)
      case f: Stmt.If => resolveIfStmt(state, f)
      case p: Stmt.Print => resolvePrintStmt(state, p)
      case r: Stmt.Return => resolveReturnStmt(state, r)
      case v: Stmt.Var => resolveVarStmt(state, v)
      case w: Stmt.While => resolveWhileStmt(state, w)
    }
  }

  private def resolveFunction(state: State, stmt: Stmt.Function, functionType: FunctionType): Either[ResolverError, State] = {
    debug(stmt, s"Entering function ${stmt.name.lexeme}")
    val result = state.functionBody(functionType) { functionContextState =>
      functionContextState.scoped { functionState =>
        val parametersState = stmt.parameters.foldLeft(functionState)(
          (state, token) => state.declare(token).define(token)
        )
        resolve(parametersState, stmt.body)
      }
    }
    debug(stmt, s"Leaving function ${stmt.name.lexeme}")
    result
  }

  private def resolveBlockStmt(state: State, stmt: Stmt.Block): Either[ResolverError, State] = {
    debug(stmt, s"Entering block ")
    val result = state.scoped(resolve(_, stmt.statements))
    debug(stmt, s"Leaving block")
    result
  }

  private def resolveBreakStmt(state: State, stmt: Stmt.Break): Either[ResolverError, State] = {
    Right(state)
  }

  private def resolveClassStmt(state: State, stmt: Stmt.Class): Either[ResolverError, State] = {
    val nameState = state.declare(stmt.name).define(stmt.name)
    nameState.classBody(ClassType.Class) { classState =>
      classState.scoped { innerState =>
        val thisState = innerState.define(Token.thisToken(stmt.line))
        stmt.methods.foldLeft[Either[ResolverError, State]](Right(thisState)) { (state, method) =>
          val functionType =
            if (method.name.lexeme == "init") FunctionType.Initializer
            else FunctionType.Method
          state.flatMap(resolveFunction(_, method, functionType))
        }
      }
    }
  }

  private def resolveContinueStmt(state: State, stmt: Stmt.Continue): Either[ResolverError, State] = {
    Right(state)
  }

  private def resolveExpressionStmt(state: State, stmt: Stmt.Expression): Either[ResolverError, State] = {
    resolve(state, stmt.expression)
  }

  private def resolveFunctionStmt(state: Resolver.State, stmt: Stmt.Function): Either[ResolverError, State] = {
    val nameState = state.declare(stmt.name).define(stmt.name)
    resolveFunction(nameState, stmt, FunctionType.Function)
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

  private def resolveWhileStmt(state: State, stmt: Stmt.While): Either[ResolverError, State] = {
    resolve(state, stmt.condition).flatMap(resolve(_, stmt.body))
  }

  private def resolve(state: State, expr: Expr): Either[ResolverError, State] = {
    expr match {
      case a: Expr.Assign => resolveAssignExpr(state, a)
      case b: Expr.Binary => resolveBinaryExpr(state, b)
      case c: Expr.Call => resolveCallExpr(state, c)
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

  private def resolveLocal(state: State, expr: Expr, name: Token): Either[ResolverError, State] = {
    Right(state.scopes.reverse.zipWithIndex.reverse.collectFirst {
      case (scope, idx) if scope.contains(name.lexeme) =>
        debug(name, s"Found local var ${name.lexeme} in scope ${idx + 1}")
        idx
    }.map({ idx =>
      val depth = state.scopes.length - 1 - idx
      debug(name, s"Marking ${name.lexeme} resolved at depth $depth for ${expr.getClass.getSimpleName} ${System.identityHashCode(expr)}")
      state.resolve(expr, depth)
    }).getOrElse {
      debug(name, s"Failed to find var ${name.lexeme}; assuming it's global (scope depth is ${state.scopes.length})")
      state
    })
  }

  private def resolveAssignExpr(state: State, expr: Expr.Assign): Either[ResolverError, State] = {
    resolve(state, expr.value).flatMap(resolveLocal(_, expr, expr.name))
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
      resolveLocal(state, expr, expr.keyword)
    }
  }

  private def resolveUnaryExpr(state: State, stmt: Expr.Unary): Either[ResolverError, State] = {
    resolve(state, stmt.right)
  }

  private def resolveVariableExpr(state: State, expr: Expr.Variable): Either[ResolverError, State] = {
    state.scopes match {
      case head :: _ if head.get(expr.name.lexeme).contains(false) =>
        Left(ResolverError(expr.name, "Cannot read local variable in its own initializer."))
      case _ =>
        resolveLocal(state, expr, expr.name)
    }
  }
}
