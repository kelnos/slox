package org.spurint.slox.parser

import org.spurint.slox.model.LiteralValue.{BooleanValue, NilValue}
import org.spurint.slox.scanner.Token
import org.spurint.slox.util._
import scala.annotation.tailrec

object RecursiveDescentParser extends LoxLogger {
  case class ParserError(expected: Seq[Token.Type], actual: Seq[Token])

  private case class State(tokens: Seq[Token], stmts: Seq[Stmt], errors: Seq[ParserError])

  def apply(tokens: Seq[Token]): Either[Seq[ParserError], Seq[Stmt]] = {
    @tailrec
    def rec(state: State): State = {
      state.tokens match {
        case Token(Token.Type.Eof, _, _) :: Nil => state
        case _ => declaration(state.tokens) match {
          case Right((stmt, tail)) => rec(state.copy(tokens = tail, stmts = state.stmts :+ stmt))
          case Left(error) => rec(state.copy(tokens = synchronize(error.actual), errors = state.errors :+ error))
        }
      }
    }

    val finalState = rec(State(tokens, Seq.empty, Seq.empty))
    if (finalState.errors.nonEmpty) Left(finalState.errors)
    else Right(finalState.stmts)
  }

  @tailrec
  private def synchronize(tokens: Seq[Token]): Seq[Token] = {
    debug(s"attempt to resync at ${tokens.headOption}")
    tokens.headOption match {
      case Some(token) =>
        token.`type` match {
          case Token.Type.Semicolon =>
            tokens.tail
          case Token.Type.Class |
               Token.Type.Fun |
               Token.Type.Var |
               Token.Type.For |
               Token.Type.If |
               Token.Type.While |
               Token.Type.Print |
               Token.Type.Return |
               Token.Type.Eof =>
            tokens
          case _ =>
            synchronize(tokens.tail)
        }
      case _ => tokens
    }
  }

  private def discard(`type`: Token.Type, tokens: Seq[Token]): Either[ParserError, Seq[Token]] = {
    tokens.headOption match {
      case Some(Token(`type`, _, _)) => Right(tokens.tail)
      case _ => Left(ParserError(Seq(`type`), tokens))
    }
  }

  private def consume(`type`: Token.Type, tokens: Seq[Token]): Either[ParserError, (Token, Seq[Token])] = {
    tokens.headOption match {
      case Some(token @ Token(`type`, _, _)) => Right(token -> tokens.tail)
      case _ => Left(ParserError(Seq(`type`), tokens))
    }
  }

  private def isNextToken(`type`: Token.Type, tokens: Seq[Token]): Boolean = {
    tokens.headOption match {
      case Some(Token(`type`, _, _)) => true
      case _ => false
    }
  }

  private def declaration(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    tokens.headOption match {
      case Some(Token(Token.Type.Class, _, _)) => classDeclaration(tokens.tail)
      case Some(Token(Token.Type.Fun, _, _)) if isNextToken(Token.Type.Identifier, tokens.tail) => function(tokens.tail)
      case Some(Token(Token.Type.Var, _, _)) => varDeclaration(tokens)
      case _ => statement(tokens)
    }
  }

  private object classDeclaration {
    private case class ClassBody(staticMethods: List[Stmt.Function], methods: List[Stmt.Function], getters: List[Stmt.Function])

    def apply(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
      for {
        nameRes <- consume(Token.Type.Identifier, tokens)
        (name, tail1) = nameRes
        _ = debug(name, s"Found class definition for ${name.lexeme}")
        tail2 <- discard(Token.Type.LeftBrace, tail1)
        methodsRes <- classBody(ClassBody(Nil, Nil, Nil), tail2)
        (ClassBody(staticMethods, methods, getters), tail3) = methodsRes
        _ = debug(methods.lastOption.getOrElse(name), s"Got ${methods.length} methods for class ${name.lexeme}")
        tail4 <- discard(Token.Type.RightBrace, tail3)
      } yield (Stmt.Class(name, staticMethods, methods, getters), tail4)
    }

    @tailrec
    private def classBody(body: ClassBody,  tokens: Seq[Token]): Either[ParserError, (ClassBody, Seq[Token])] = {
      tokens.headOption match {
        case Some(Token(Token.Type.RightBrace, _, _)) =>
          debug("Found end of method list")
          Right((body, tokens))
        case Some(Token(Token.Type.Fun, _, _)) =>
          function(tokens.tail) match {
            case Right((stmt, tail)) =>
              debug(stmt, s"Found static method ${stmt.name.lexeme}")
              classBody(body.copy(staticMethods = stmt :: body.staticMethods), tail)
            case l @ Left(_) => l.rightCast
          }
        case Some(token @ Token(Token.Type.Identifier, _, _)) =>
          discard(Token.Type.LeftBrace, tokens.tail) match {
            case Right(tail1) => block(tail1) match {
              case Right((stmts, tail2)) =>
                debug(token, s"Found getter ${token.lexeme}")
                val getter = Stmt.Function(token, Expr.Function(token, Seq.empty, stmts))
                classBody(body.copy(getters = getter :: body.getters), tail2)
              case l @ Left(_) => l.rightCast
            }
            case Left(_) => function(tokens) match {
              case Right((stmt, tail)) =>
                debug(stmt, s"Found method ${stmt.name.lexeme}")
                classBody(body.copy(methods = stmt :: body.methods), tail)
              case l @ Left(_) => l.rightCast
            }
          }
        case Some(token) => Left(ParserError(Seq(Token.Type.RightBrace, Token.Type.Fun, Token.Type.Identifier), Seq(token)))
        case _ => Left(ParserError(Seq(Token.Type.RightBrace, Token.Type.Fun, Token.Type.Identifier), Seq.empty))
      }
    }
  }

  private def function(tokens: Seq[Token]): Either[ParserError, (Stmt.Function, Seq[Token])] = {
    for {
      nameRes <- consume(Token.Type.Identifier, tokens)
      (name, tail1) = nameRes
      _ = debug(name, "consumed identifier")
      functionBodyRes <- functionBody(tail1)
      (functionBody, tail2) = functionBodyRes
    } yield (Stmt.Function(name, functionBody), tail2)
  }

  private object functionBody {
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr.Function, Seq[Token])] = {
      (for {
        parenRes <- consume(Token.Type.LeftParen, tokens)
        (parenToken, tail1) = parenRes
        _ = debug(parenToken, "consumed left paren")
        paramsRes <- consumeParameters(Seq.empty[Token], tail1)
        (params, tail2) = paramsRes
        tail3 <- discard(Token.Type.LeftBrace, tail2)
        bodyRes <- block(tail3)
        (body, tail4) =bodyRes
      } yield {
        (Expr.Function(parenToken, params, body), tail4)
      }).flatMap {
        case (function, tail) if function.parameters.length > MAX_CALL_ARGS =>
          // FIXME: this gives a not-very-useful error message
          Left(ParserError(Seq(Token.Type.RightParen), tail))
        case r => Right(r)
      }
    }

    @tailrec
    private def consumeParameters(params: Seq[Token], tokens: Seq[Token]): Either[ParserError, (Seq[Token], Seq[Token])] = {
      tokens.headOption match {
        case Some(param @ Token(Token.Type.Identifier, _, _)) =>
          debug(param, s"found param ident ${param.lexeme}")
          tokens.tail.headOption match {
            case Some(Token(Token.Type.Comma, _, _)) => consumeParameters(params :+ param, tokens.tail.tail)
            case Some(Token(Token.Type.RightParen, _, _)) => Right(params :+ param, tokens.tail.tail)
            case _ => Left(ParserError(Seq(Token.Type.Comma, Token.Type.RightParen), tokens.tail))
          }
        case Some(Token(Token.Type.RightParen, _, _)) => Right(params, tokens.tail)
        case _ => Left(ParserError(Seq(Token.Type.Identifier), tokens))
      }
    }
  }

  private def varDeclaration(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    discard(Token.Type.Var, tokens).flatMap(tail => tail.headOption.collectFirst {
      case token @ Token(Token.Type.Identifier, _, _) =>
        for {
          initializerRes <- discard(Token.Type.Equal, tail.tail) match {
            case Left(_) =>
              // no assignment for this declaration
              Right(None -> tail.tail)
            case Right(tail2) =>
              // got an '=', so we have an assignment
              expression(tail2).map { case (value, tail3) => Option(value) -> tail3 }
          }
          (maybeInitializer, tail3) = initializerRes
          finalTail <- discard(Token.Type.Semicolon, tail3)
        } yield (Stmt.Var(token, maybeInitializer), finalTail)
    }.getOrElse(Left(ParserError(Seq(Token.Type.Identifier), tokens))))
  }

  private def statement(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    tokens.headOption match {
      case Some(token) =>
        token.`type` match {
          case Token.Type.Print => printStatement(tokens.tail)
          case Token.Type.LeftBrace => block(tokens.tail).map { case (stmts, tail) => (Stmt.Block(stmts), tail) }
          case Token.Type.If => ifStatement(tokens.tail)
          case Token.Type.While => whileStatement(tokens.tail)
          case Token.Type.For => forStatement(tokens.tail)
          case Token.Type.Return => returnStatement(token, tokens.tail)
          case Token.Type.Break => breakStatement(token, tokens.tail)
          case Token.Type.Continue => continueStatement(token, tokens.tail)
          case _ => expressionStatement(tokens)
        }
      case _ => expressionStatement(tokens)
    }
  }

  @tailrec
  private def block(tokens: Seq[Token], statements: Seq[Stmt] = Seq.empty[Stmt]): Either[ParserError, (Seq[Stmt], Seq[Token])] = {
    tokens.headOption match {
      case Some(token) =>
        token.`type` match {
          case Token.Type.RightBrace => Right(statements -> tokens.tail)
          case _ => declaration(tokens) match {
            case Right((decl, tail)) => block(tail, statements :+ decl)
            case l @ Left(_) => l.rightCast
          }
        }
      case _ => Left(ParserError(Seq(Token.Type.RightBrace), Seq.empty[Token]))
    }
  }

  private def expressionStatement(tokens: Seq[Token]): Either[ParserError, (Stmt.Expression, Seq[Token])] = {
    for {
      res <- expression(tokens)
      (expression, tail) = res
      finalTail <- discard(Token.Type.Semicolon, tail)
    } yield (Stmt.Expression(expression), finalTail)
  }

  private def printStatement(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    for {
      res <- expression(tokens)
      (expression, tail) = res
      finalTail <- discard(Token.Type.Semicolon, tail)
    } yield (Stmt.Print(expression), finalTail)
  }

  private def ifStatement(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    for {
      tail1 <- discard(Token.Type.LeftParen, tokens)
      conditionRes <- expression(tail1)
      (condition, tail2) = conditionRes
      tail3 <- discard(Token.Type.RightParen, tail2)

      thenBranchRes <- statement(tail3)
      (thenBranch, tail4) = thenBranchRes

      elseBranchRes <- tail4.headOption.collectFirst {
        case Token(Token.Type.Else, _, _) => statement(tail4.tail).map { case (stmt, tail) => (Option(stmt), tail) }
      }.getOrElse(Right(None, tail4))
      (elseBranch, tail5) = elseBranchRes
    } yield {
      (Stmt.If(condition, thenBranch, elseBranch), tail5)
    }
  }

  private def whileStatement(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    for {
      tail1 <- discard(Token.Type.LeftParen, tokens)
      conditionRes <- expression(tail1)
      (condition, tail2) = conditionRes
      tail3 <- discard(Token.Type.RightParen, tail2)
      bodyRes <- statement(tail3)
      (body, tail4) = bodyRes
    } yield {
      (Stmt.For(initializer = None, condition, increment = None, body), tail4)
    }
  }

  private def forStatement(tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    for {
      tail1 <- discard(Token.Type.LeftParen, tokens)

      initializerRes <- discard(Token.Type.Semicolon, tail1)
        .map(tail => (None, tail))  // no initializer
        .recoverWith { case _ =>  // if we didn't find a semicolon, then there's an initializer
          tail1.headOption
            .collectFirst { case Token(Token.Type.Var, _, _) => varDeclaration(tail1) }
            .getOrElse(expressionStatement(tail1))
            .map { case (stmt, tail) => Some(stmt) -> tail }
        }
      (initializer, tail2) = initializerRes

      conditionRes <- discard(Token.Type.Semicolon, tail2)
        .map(tail => (Expr.Literal(BooleanValue(true)), tail)) // no condition; defaults to 'true'
        .recoverWith { case _ =>  // if we didn't find a semicolon, then there's a condition
          expressionStatement(tail2).map { case (stmt, tail) => stmt.expression -> tail }
        }
      (condition, tail3) = conditionRes

      incrementRes <- discard(Token.Type.RightParen, tail3)
        .map(tail => (None, tail))  // no condition
        .recoverWith { case _ =>  // if we didn't find a semicolon, then there's an increment
          expression(tail3).flatMap { case (expr, tail) =>
            discard(Token.Type.RightParen, tail).map(tail1 => Some(expr) -> tail1)
          }
        }
      (increment, tail4) = incrementRes

      bodyRes <- statement(tail4)
      (body, tail5) = bodyRes
    } yield (Stmt.Block(Seq(Stmt.For(initializer, condition, increment.map(Stmt.Expression.apply), body))), tail5)
  }

  private def returnStatement(returnToken: Token, tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    discard(Token.Type.Semicolon, tokens).map { tail =>
      (Stmt.Return(returnToken, Expr.Literal(NilValue)), tail)
    }.recoverWith { case _ =>
      expression(tokens).flatMap { case (returnExpr, tail) =>
        discard(Token.Type.Semicolon, tail).map { tail1 =>
          (Stmt.Return(returnToken, returnExpr), tail1)
        }
      }
    }
  }

  private def breakStatement(breakToken: Token, tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    discard(Token.Type.Semicolon, tokens).map(tail => (Stmt.Break(breakToken), tail))
  }

  private def continueStatement(continueToken: Token, tokens: Seq[Token]): Either[ParserError, (Stmt, Seq[Token])] = {
    discard(Token.Type.Semicolon, tokens).map(tail => (Stmt.Continue(continueToken), tail))
  }

  // this is used only for error display, not actual program logic
  private lazy val primaryTokenTypes = Seq[Token.Type](
    Token.Type.False,
    Token.Type.True,
    Token.Type.Nil,
    Token.Type.Number,
    Token.Type.String,
    Token.Type.LeftParen,
    Token.Type.Identifier,
  )

  @inline
  private def primary(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    tokens.headOption match {
      case Some(token @ Token(Token.Type.False | Token.Type.True | Token.Type.Nil | Token.Type.Number | Token.Type.String, _, literal)) =>
        literal match {
          case Some(lit) => Right((Expr.Literal(lit), tokens.tail))
          case _ => Left(ParserError(Seq(token.`type`), Seq.empty[Token]))
        }
      case Some(token @ Token(Token.Type.This, _, _)) =>
        Right((Expr.This(token), tokens.tail))
      case Some(token @ Token(Token.Type.Identifier, _, _)) =>
        Right((Expr.Variable(token), tokens.tail))
      case Some(token) if token.`type` == Token.Type.LeftParen =>
        for {
          res <- expression(tokens.tail)
          (expr, tail) = res
          finalTail <- discard(Token.Type.RightParen, tail)
        } yield {
          (Expr.Grouping(expr), finalTail)
        }
      case Some(Token(Token.Type.Fun, _, _)) =>
        functionBody(tokens.tail)
      case _ => Left(ParserError(primaryTokenTypes, tokens))
    }
  }

  @inline
  private def expression(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    assignment(tokens)
  }

  private def assignment(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    or(tokens).flatMap { case (expr, tail) =>
      tail.headOption match {
        case Some(Token(Token.Type.Equal, _, _)) =>
          assignment(tail.tail).flatMap { case (value, finalTail) =>
            expr match {
              case v: Expr.Variable => Right((Expr.Assign(v.name, value), finalTail))
              case g: Expr.Get => Right((Expr.Set(g.obj, g.name, value), finalTail))
              case _ => Left(ParserError(Seq(Token.Type.Identifier), tail.tail))
            }
          }
        case _ => Right((expr, tail))
      }
    }
  }

  private def or(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    and(tokens).flatMap { case (expr, tail) =>
      tail.headOption.collectFirst {
        case operator @ Token(Token.Type.Or, _, _) =>
          and(tail.tail).map { case (right, tail1) => (Expr.Logical(expr, operator, right), tail1) }
      }.getOrElse(Right(expr, tail))
    }
  }

  private def and(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
    equality(tokens).flatMap { case (expr, tail) =>
      tail.headOption.collectFirst {
        case operator @ Token(Token.Type.And, _, _) =>
          equality(tail.tail).map { case (right, tail1) => (Expr.Logical(expr, operator, right), tail1) }
      }.getOrElse(Right(expr, tail))
    }
  }

  private object unary {
    private val unaryOperators = Set[Token.Type](
      Token.Type.Bang,
      Token.Type.Minus,
    )

    private val disallowedBinaryOperators = Set[Token.Type](
      Token.Type.Plus,
      Token.Type.Star,
      Token.Type.Slash,
      Token.Type.Equal,
      Token.Type.EqualEqual,
      Token.Type.BangEqual,
      Token.Type.Greater,
      Token.Type.GreaterEqual,
      Token.Type.Less,
      Token.Type.LessEqual,
      Token.Type.And,
      Token.Type.Or,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(operator) if unaryOperators.contains(operator.`type`) =>
          unary(tokens.tail).map { case (right, tail) =>
            (Expr.Unary(operator, right), tail)
          }
        case Some(operator) if disallowedBinaryOperators.contains(operator.`type`) =>
          Left(ParserError(unaryOperators.toSeq, tokens))
        case _ => call(tokens)
      }
    }
  }

  private object multiplication {
    private val multiplicationOperators = Set[Token.Type](
      Token.Type.Slash,
      Token.Type.Star,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      unary(tokens).flatMap { case (left, tail) => multiplicationRec(left, tail) }
    }

    @tailrec
    private def multiplicationRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(operator) if multiplicationOperators.contains(operator.`type`) =>
          unary(tokens.tail) match {
            case Right((right, tail)) => multiplicationRec(Expr.Binary(left, operator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object addition {
    private val additionOperators = Set[Token.Type](
      Token.Type.Minus,
      Token.Type.Plus,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      multiplication(tokens).flatMap { case (left, tail) => additionRec(left, tail) }
    }

    @tailrec
    private def additionRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(operator) if additionOperators.contains(operator.`type`) =>
          multiplication(tokens.tail) match {
            case Right((right, tail)) => additionRec(Expr.Binary(left, operator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object comparison {
    private val comparisonOperators = Set[Token.Type](
      Token.Type.Greater,
      Token.Type.GreaterEqual,
      Token.Type.Less,
      Token.Type.LessEqual,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      addition(tokens).flatMap { case (left, tail) => comparisonRec(left, tail) }
    }

    @tailrec
    private def comparisonRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(comparator) if comparisonOperators.contains(comparator.`type`) =>
          addition(tokens.tail) match {
            case Right((right, tail)) => comparisonRec(Expr.Binary(left, comparator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object equality {
    private val equalityOperators = Set[Token.Type](
      Token.Type.BangEqual,
      Token.Type.EqualEqual,
    )

    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      comparison(tokens).flatMap { case (left, tail) => equalityRec(left, tail) }
    }

    @tailrec
    private def equalityRec(left: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(comparator) if equalityOperators.contains(comparator.`type`) =>
          comparison(tokens.tail) match {
            case Right((right, tail)) => equalityRec(Expr.Binary(left, comparator, right), tail)
            case l => l
          }
        case _ => Right(left, tokens)
      }
    }
  }

  private object call {
    @inline
    def apply(tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      primary(tokens).flatMap { case (expr, tail) => callRec(expr, tail) }
    }

    @tailrec
    private def callRec(expr: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(Token(Token.Type.LeftParen, _, _)) =>
          finishCall(expr, tokens.tail) match {
            case Right((expr1, tail1)) => callRec(expr1, tail1)
            case l => l
          }
        case Some(Token(Token.Type.Dot, _, _)) =>
          consume(Token.Type.Identifier, tokens.tail) match {
            case Right((name, tail1)) => callRec(Expr.Get(expr, name), tail1)
            case l @ Left(_) => l.rightCast
          }
        case _ => Right(expr -> tokens)
      }
    }

    @inline
    private def finishCall(callee: Expr, tokens: Seq[Token]): Either[ParserError, (Expr, Seq[Token])] = {
      tokens.headOption match {
        case Some(token @ Token(Token.Type.RightParen, _, _)) =>
          Right(Expr.Call(callee, token, Seq.empty[Expr]) -> tokens.tail)
        case _ =>
          consumeArguments(Seq.empty[Expr], tokens).flatMap { case (args, tail) =>
            if (args.length > MAX_CALL_ARGS) {
              Left(ParserError(Seq(Token.Type.RightParen), tokens)) // FIXME: this gives a not-very-useful error message
            } else {
              tail.headOption.collectFirst {
                case token @ Token(Token.Type.RightParen, _, _) => Right(token)
              }.getOrElse(Left(ParserError(Seq(Token.Type.RightParen), tail))).map { closingParen =>
                (Expr.Call(callee, closingParen, args), tail.tail)
              }
            }
          }
      }
    }

    @tailrec
    private def consumeArguments(args: Seq[Expr], tokens: Seq[Token]): Either[ParserError, (Seq[Expr], Seq[Token])] = {
      expression(tokens) match {
        case Right((arg, tail)) => tail.headOption match {
          case Some(Token(Token.Type.Comma, _, _)) => consumeArguments(args :+ arg, tail.tail)
          case Some(Token(Token.Type.RightParen, _, _)) => Right(args :+ arg, tail)
          case _ => Left(ParserError(Seq(Token.Type.Comma, Token.Type.RightParen), tail))
        }
        case l @ Left(_) => l.rightCast
      }
    }
  }
}
