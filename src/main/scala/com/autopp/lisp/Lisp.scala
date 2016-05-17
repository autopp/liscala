package com.autopp.lisp

import scala.collection.mutable.Map

class Lisp {
  type Result = Lisp.Result

  val env = initialEnv
  def eval(source: String): Result = {
    new Parser().parse(source) match {
      case Left(msg) => Left(msg)
      case Right(sexpr) => evalSExpr(sexpr, env)
    }
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
        toList(cdr, "function call must be list") match {
          case Right(list) => {
            evalSExpr(car, env) match {
              case l @ Left(_) => l
              case Right(proc: Proc) => applyProc(proc, list, env)
              case Right(notProc) => Left(s"invalid application")
            }
          }
          case Left(msg) => Left(msg)
        }
      }
      case _ => Left("BUG: expect atom or list")
    }
  }

  def applyProc(proc: Proc, args: List[SExpr], env: Env): Result = {
    val arity = proc.arity
    val argc = args.length

    if (proc.varg) {
      if (argc < arity) return Left(s"expect ${arity} or more than arguments, but got ${argc}")
    }
    else {
      if (argc != arity) return Left(s"expect ${arity} arguments, but got ${argc}")
    }

    proc match {
      case SpecialForm(_, _, _, body) => body(args, env)
      case func: Func => {
        def evalArgs(args: List[SExpr], env: Env, buf: List[SExpr]): Either[String, List[SExpr]] = {
          args match {
            case Nil => Right(buf.reverse)
            case arg::rest => {
              evalSExpr(arg, env) match {
                case Left(msg) => Left(msg)
                case Right(sexpr) => evalArgs(rest, env, sexpr::buf)
              }
            }
          }
        }

        evalArgs(args, env, Nil) match {
          case Left(msg) => Left(msg)
          case Right(evaluatedArgs) => applyFunc(func, evaluatedArgs)
        }
      }
    }
  }

  def applyFunc(func: Func, args: List[SExpr]): Result = {
    func match {
      case UserFunc(_, params, env, body) => {
        val newEnv = new Env(Map() ++ params.zip(args).toMap, Some(env))
        evalSExpr(body, newEnv)
      }
      case BuiltinFunc(_, _, _, body) => body(args)
    }
  }

  def isList(sexpr: SExpr): Boolean = {
    sexpr match {
      case NilVal => true
      case Pair(_, cdr) => isList(cdr)
      case _ => false
    }
  }

  def toList(sexpr: SExpr, errMsg: String): Either[String, List[SExpr]] = {
    def toListWithBuf(sexpr: SExpr, buf: List[SExpr]): Either[String, List[SExpr]] = {
      sexpr match {
        case NilVal => Right(buf.reverse)
        case Pair(car, cdr) => toListWithBuf(cdr, car::buf)
        case _ => Left(errMsg)
      }
    }

    toListWithBuf(sexpr, Nil)
  }

  def initialEnv: Env = {
    def specialForm(name: String, arity: Int, varg: Boolean)(body: (List[SExpr], Env) => Result): (String, SpecialForm) = {
      (name, SpecialForm(name, arity, varg, body))
    }
    def builtinFunc(name: String, arity: Int, varg: Boolean)(body: List[SExpr] => Result): (String, BuiltinFunc) = {
      (name, BuiltinFunc(name, arity, varg, body))
    }

    val map = Map[String, SExpr](
      specialForm("if", 2, true) { (args, env) =>
        def evalIf(condExpr: SExpr, thenExpr: SExpr, elseExpr: SExpr, env: Env): Result = {
          evalSExpr(condExpr, env) match {
            case Left(msg) => Left(msg)
            case Right(False) => evalSExpr(elseExpr, env)
            case Right(_) => evalSExpr(thenExpr, env)
          }
        }

        args match {
          case condExpr::thenExpr::elseExpr::Nil => evalIf(condExpr, thenExpr, elseExpr, env)
          case condExpr::thenExpr::Nil => evalIf(condExpr, thenExpr, NilVal, env)
          case _ => Left(s"if: required 2 or 3 arguments, but got ${args.length}")
        }
      },
      specialForm("quote", 1, false) {(args, _) => Right(args.head)},
      specialForm("lambda", 2, false) {(args, env) =>
        val paramErrorMsg = "lambda: 1st argument must be list of parametor"
        val params = args(0)
        val body = args(1)

        toList(params, paramErrorMsg).right.flatMap { paramList =>
          paramList.foldRight[Either[String, List[String]]](Right(Nil)) {
            case (Sym(name), Right(list)) => Right(name::list)
            case _ => Left(paramErrorMsg)
          }.right.map { paramList => UserFunc(None, paramList, env, body) }
        }
      },
      specialForm("define", 2, false) {(args, env) =>
        val target = args(0)
        val sexpr = args(1)

        target match {
          case Sym(name) => {
            evalSExpr(sexpr, env) match {
              case Left(msg) => Left(msg)
              case Right(value) => {
                env(name) = value
                Right(Sym(name))
              }
            }
          }
          case _ => Left("define: 1st argument must be symbol")
        }
      },
      specialForm("let", 2, false) {(args, env) =>
        type BindingList = List[(String, SExpr)]
        def toBindingList(params: SExpr): Either[String, BindingList] = {
          val errMsg = "1st argument must be list of name and value pair"
          toList(params, errMsg).right.flatMap { paramList =>
            paramList.foldRight[Either[String, BindingList]](Right(Nil)) {
              case (Pair(Sym(name), Pair(sexpr, NilVal)), Right(list)) => Right((name, sexpr)::list)
              case _ => Left(errMsg)
            }
          }
        }

        def evalBindingList(bindingList: BindingList): Either[String, BindingList] = {
          bindingList.foldLeft[Either[String, BindingList]](Right(Nil)) {
            case (Left(msg), _) => Left(msg)
            case (Right(list), (name, sexpr)) => evalSExpr(sexpr, env).right.map { sexpr => (name, sexpr)::list }
          }
        }

        val params = args(0)
        val body = args(1)

        toBindingList(params).right.flatMap(evalBindingList).right.flatMap {(bindingList) =>
          val newEnv = new Env(Map() ++ bindingList, Some(env))
          evalSExpr(body, newEnv)
        }
      },
      builtinFunc("atom", 1, false) {args =>
        args.head match {
          case _: Atom => Right(True)
          case _ => Right(False)
        }
      },
      builtinFunc("cons", 2, false) {args => Right(Pair(args(0), args(1)))},
      builtinFunc("car", 1, false) {args =>
        args.head match {
          case Pair(car, _) => Right(car)
          case _ => Left("car: expected pair")
        }
      },
      builtinFunc("cdr", 1, false) {args =>
        args.head match {
          case Pair(_, cdr) => Right(cdr)
          case _ => Left("cdr: expected pair")
        }
      },
      builtinFunc("=", 2, true) {args =>
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
      builtinFunc("+", 0, true) {args =>
        def sum(list: List[SExpr], r: Int): Result = {
          list match {
            case Nil => Right(Num(r))
            case Num(n)::rest => sum(rest, n + r)
            case _::_ => Left("+: expected list of number")
          }
        }

        sum(args, 0)
      },
      builtinFunc("-", 1, true) {args =>
        def sub(list: List[SExpr], r: Int): Result = {
          list match {
            case Nil => Right(Num(r))
            case Num(n)::rest => sub(rest, r - n)
            case _::_ => Left("-: expected list of number")
          }
        }

        args.head match {
          case Num(n) => sub(args.tail, n)
          case _ => Left("-: expected list of number")
        }
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
