package com.autopp.lisp

import scala.collection.mutable.Map

class Lisp {
  type MayError[A] = Lisp.MayError[A]
  type Result = Lisp.Result

  val env = initialEnv
  def eval(source: String): Result = {
    new Parser().parse(source).right.flatMap { sexpr => evalSExpr(sexpr, env) }
  }

  def evalSExpr(sexpr: SExpr, env: Env): Lisp.Result = {
    sexpr match {
      case Sym(name) => {
        env.lookup(name) match {
          case Some(lookuped) => Right(lookuped)
          case None => Left(s"unboud variable: ${name}")
        }
      }
      case atom: Atom => Right(atom)
      case Pair(car, cdr) => {
        toList(cdr, "function call must be list").right.flatMap { list =>
          evalSExpr(car, env).right.flatMap {
            case proc: Proc => applyProc(proc, list, env)
            case _ => Left(s"invalid application")
          }
        }
      }
      case _ => Left("BUG: expect atom or list")
    }
  }

  def applyProc(proc: Proc, args: List[SExpr], env: Env): Result = {
    val argc = args.length

    proc.arity match {
      case FixedArity(size) if argc != size => return Left(s"expect ${size} arguments, but got ${argc}")
      case RangeArity(min, max) if argc < min || argc > max => return Left(s"expect ${min} to ${max} arguments, but got ${argc}")
      case VariableArity(min) if argc < min => return Left(s"expect ${min} arguments, but got ${argc}")
      case _ => ()
    }

    proc match {
      case SpecialForm(_, _, body) => body(args, env)
      case func: Func => {
        args.foldLeft[MayError[List[SExpr]]](Right(Nil)) {
          case (l @ Left(_), _) => l
          case (Right(list), arg) => evalSExpr(arg, env).right.map{ sexpr => sexpr::list }
        }.right.flatMap { evaluatedArgs => applyFunc(func, evaluatedArgs.reverse) }
      }
    }
  }

  def applyFunc(func: Func, args: List[SExpr]): Result = {
    func match {
      case UserFunc(_, params, env, body) => {
        val newEnv = new Env(params.zip(args).toMap, Some(env))
        evalSExpr(body, newEnv)
      }
      case BuiltinFunc(_, _, body) => body(args)
    }
  }

  def toList(sexpr: SExpr, errMsg: String): MayError[List[SExpr]] = {
    def toListWithBuf(sexpr: SExpr, buf: List[SExpr]): MayError[List[SExpr]] = {
      sexpr match {
        case NilVal => Right(buf.reverse)
        case Pair(car, cdr) => toListWithBuf(cdr, car::buf)
        case _ => Left(errMsg)
      }
    }

    toListWithBuf(sexpr, Nil)
  }

  def initialEnv: Env = {
    def specialForm(name: String, arity: Arity)(body: (List[SExpr], Env) => Result): (String, SpecialForm) = {
      (name, SpecialForm(name, arity, body))
    }
    def builtinFunc(name: String, arity: Arity)(body: List[SExpr] => Result): (String, BuiltinFunc) = {
      (name, BuiltinFunc(name, arity, body))
    }

    val map = Map[String, SExpr](
      specialForm("if", RangeArity(2, 3)) { (args, env) =>
        val condExpr = args(0)
        val thenExpr = args(1)
        val elseExpr = if (args.isDefinedAt(2)) args(2) else NilVal

        evalSExpr(condExpr, env).right.flatMap {
          case False => evalSExpr(elseExpr, env)
          case _ => evalSExpr(thenExpr, env)
        }
      },
      specialForm("quote", FixedArity(1)) {(args, _) => Right(args.head)},
      specialForm("lambda", FixedArity(2)) {(args, env) =>
        val paramErrorMsg = "lambda: 1st argument must be list of parametor"
        val params = args(0)
        val body = args(1)

        toList(params, paramErrorMsg).right.flatMap { paramList =>
          paramList.foldRight[MayError[List[String]]](Right(Nil)) {
            case (Sym(name), Right(list)) => Right(name::list)
            case _ => Left(paramErrorMsg)
          }.right.map { paramList => UserFunc(None, paramList, env, body) }
        }
      },
      specialForm("define", FixedArity(2)) {(args, env) =>
        val target = args(0)
        val sexpr = args(1)

        target match {
          case sym @ Sym(name) => {
            evalSExpr(sexpr, env).right.map { value =>
              env(name) = value
              sym
            }
          }
          case _ => Left("define: 1st argument must be symbol")
        }
      },
      specialForm("let", FixedArity(2)) {(args, env) =>
        type BindingList = List[(String, SExpr)]
        val errMsg = "1st argument must be list of name and value pair"

        val params = args(0)
        val body = args(1)

        val bindingList = toList(params, errMsg).right.flatMap { paramList =>
          paramList.foldRight(Right(Nil): MayError[BindingList]) {
            case (Pair(Sym(name), Pair(sexpr, NilVal)), Right(list)) => Right((name, sexpr)::list)
            case _ => Left(errMsg)
          }
        }

        val evaluatedBindingList = bindingList.right.flatMap {
          _.foldLeft(Right(Nil): MayError[BindingList]) {
            case (l @ Left(_), _) => l
            case (Right(list), (name, sexpr)) => evalSExpr(sexpr, env).right.map { (name, _)::list }
          }
        }

        evaluatedBindingList.right.flatMap {(bindingList) =>
          val newEnv = new Env(bindingList.toMap, Some(env))
          evalSExpr(body, newEnv)
        }
      },
      builtinFunc("atom", FixedArity(1)) {args =>
        args.head match {
          case _: Atom => Right(True)
          case _ => Right(False)
        }
      },
      builtinFunc("cons", FixedArity(2)) {args => Right(Pair(args(0), args(1)))},
      builtinFunc("car", FixedArity(1)) {args =>
        args.head match {
          case Pair(car, _) => Right(car)
          case _ => Left("car: expected pair")
        }
      },
      builtinFunc("cdr", FixedArity(1)) {args =>
        args.head match {
          case Pair(_, cdr) => Right(cdr)
          case _ => Left("cdr: expected pair")
        }
      },
      builtinFunc("=", VariableArity(2)) {args =>
        def evalEqualNum(n: Int, others: List[SExpr]): Result = {
          others match {
            case Nil => Right(True)
            case Num(other)::rest => if (n == other) evalEqualNum(n, rest) else Right(False)
            case _ => Left("=: expected list of number")
          }
        }
        args.head match {
          case Num(n) => evalEqualNum(n, args.tail)
          case _ => Left("=: expected list of number")
        }
      },
      builtinFunc("+", VariableArity(0)) {
        _.foldLeft[MayError[Int]](Right(0)) {
          case (Right(r), Num(n)) => Right(r + n)
          case _ => Left("+: expected list of number")
        }.right.map(Num(_))
      },
      builtinFunc("-", VariableArity(1)) {args =>
        val errMsg = "-: expected list of number"
        val r = args.head match {
          case Num(x) => args.tail.foldLeft[MayError[Int]](Right(x)) {
            case (Right(r), Num(n)) => Right(r - n)
            case _ => Left(errMsg)
          }
          case _ => Left(errMsg)
        }
        r.right.map(Num(_))
      }
    )

    new Env(map, None)
  }
}

object Lisp {
  type MayError[A] = Either[String, A]
  type Result = MayError[SExpr]

  def main(args: Array[String]) {
    val lisp = new Lisp
    for (str <- args) {
      lisp.eval(str) match {
        case Left(msg) => {
          System.err.println(s"ERROR: ${msg}")
          sys.exit(1)
        }
        case Right(sexpr) => println(sexpr.toString)
      }
    }
  }
}
