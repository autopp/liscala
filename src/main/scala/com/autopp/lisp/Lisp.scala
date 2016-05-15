package com.autopp.lisp

class Lisp {
  def eval(source: String): SExpr = NilVal
}

object Lisp {
  type Result = Either[String, SExpr]
}
