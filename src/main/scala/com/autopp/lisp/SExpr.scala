package com.autopp.lisp

import com.autopp.lisp.Lisp.Result

sealed abstract class SExpr

abstract class Atom extends SExpr
case object NilVal extends Atom {
  override def toString = "()"
}
case class Sym(s: String) extends Atom {
  override def toString = s
}
case class Num(n: Int) extends Atom {
  override def toString = n.toString
}

abstract class Bool extends Atom
case object True extends Bool {
  override def toString = "#t"
}
case object False extends Bool {
  override def toString = "#f"
}

case class Pair(var car: SExpr, var cdr: SExpr) extends SExpr {
  override def toString = {
    def buildString(sexpr: SExpr, buf: StringBuilder): StringBuilder = {
      sexpr match {
        case Pair(car, cdr) => {
          buf.append(" ")
          buf.append(car.toString)
          buildString(cdr, buf)
        }
        case NilVal => {
          buf.append(")")
        }
        case _ => {
          buf.append(s" . ${sexpr.toString})")
          buf
        }
      }
    }

    val buf = new StringBuilder(s"(${car}")
    buildString(cdr, buf).toString
  }
}

abstract class Proc(val arity: Int, val varg: Boolean) extends SExpr
case class SpecialForm(
  name: String, override val arity: Int, override val varg: Boolean, body: (List[SExpr], Env) => Result) extends Proc(arity, varg) {
  override def toString = s"#<syntax ${name}>"
}

abstract class Func(override val arity: Int, override val varg: Boolean) extends Proc(arity, varg)
case class UserFunc (
  name: Option[String], params: List[String], env: Env, body: SExpr) extends Func(params.length, false) {
  override def toString = {
    name match {
      case Some(s) => s"#<lambda ${s}>"
      case None => "#<lambda>"
    }
  }
}

case class BuiltinFunc (
  name: String, override val arity: Int, override val varg: Boolean, body: List[SExpr] => Result) extends Func(arity, varg) {
  override def toString = s"#<builtin ${name}>"
}
