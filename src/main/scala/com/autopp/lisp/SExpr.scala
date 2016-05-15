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

abstract class Proc extends SExpr
case class SpecialForm(
  name: String, arity: Int, varg: Boolean, body: (Seq[SExpr]) => Result) extends Proc

abstract class Func extends Proc
case class UserFunc (
  name: Option[String], arity: Int, varg: Boolean, body: SExpr, env: Env) extends Func

case class BuiltinFunc (
  name: Option[String], arity: Int, varg: Boolean, body: (Seq[SExpr]) => Result) extends Func
