package com.autopp.lisp

import com.autopp.lisp.Lisp.Result

sealed abstract class SExpr

abstract class Atom extends SExpr
case object NilVal extends Atom
case class Sym(s: String) extends Atom
case class Num(n: Int) extends Atom

abstract class Bool extends Atom
case object True extends Bool
case object False extends Bool

case class Pair(var car: SExpr, var cdr: SExpr) extends SExpr

abstract class Proc(val arity: Int, val varg: Boolean) extends SExpr
case class SpecialForm(
  name: String, override val arity: Int, override val varg: Boolean, body: (Seq[SExpr], Env) => Result) extends Proc(arity, varg)

abstract class Func(override val arity: Int, override val varg: Boolean) extends Proc(arity, varg)
case class UserFunc (
  name: Option[String], params: List[String], env: Env, body: SExpr) extends Func(params.length, false)

case class BuiltinFunc (
  name: Option[String], override val  arity: Int, override val  varg: Boolean, body: (Seq[SExpr]) => Result) extends Func(arity, varg)
