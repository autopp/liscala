package com.autopp.lisp

import scala.collection.mutable.Map

class Lisp {
  type Result = Lisp.Result

  val env = new Env(Map(), None)
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
        toList(cdr) match {
          case Some(list) => {
            evalSExpr(car, env) match {
              case Left(msg) => Left(msg)
              case Right(proc: Proc) => applyProc(proc, list, env)
              case Right(notProc) => Left(s"invalid application")
            }
          }
          case None => Left("function call must be list")
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

  def toList(sexpr: SExpr): Option[List[SExpr]] = {
    def toListWithBuf(sexpr: SExpr, buf: List[SExpr]): Option[List[SExpr]] = {
      sexpr match {
        case NilVal => Some(buf.reverse)
        case Pair(car, cdr) => toListWithBuf(cdr, car::buf)
        case _ => None
      }
    }

    toListWithBuf(sexpr, Nil)
  }
}

object Lisp {
  type Result = Either[String, SExpr]

  def main(args: Array[String]) {
    val lisp = new Lisp
    for (str <- args) {
      lisp.eval(str) match {
        case Left(msg) => {
          System.err.println(s"ERROR: ${msg}")
          sys.exit(1)
        }
        case Right(sexpr) => {
          println(sexpr.toString)
        }
      }
    }
  }
}
