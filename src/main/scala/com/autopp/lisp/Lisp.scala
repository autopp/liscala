package com.autopp.lisp

class Lisp {
  def eval(source: String): Lisp.Result = Right(NilVal)
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
