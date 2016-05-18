package com.autopp.lisp

import scala.collection.mutable.Map

class Env(val map: Map[String, SExpr], val prev: Option[Env]) {
  def this(map: scala.collection.immutable.Map[String, SExpr], prev: Option[Env]) = {
    this(Map() ++ map, prev)
  }

  def lookup(name: String): Option[SExpr] = {
    map.get(name) match {
      case Some(sexpr) => Some(sexpr)
      case None => {
        prev match {
          case Some(prevEnv) => prevEnv.lookup(name)
          case None => None
        }
      }
    }
  }

  def update(name: String, sexpr: SExpr) {
    map(name) = sexpr
  }
}
