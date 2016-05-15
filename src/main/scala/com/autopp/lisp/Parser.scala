package com.autopp.lisp

import scala.util.matching.Regex

sealed abstract class Token(val contents: String)
case class NumToken(override val contents: String) extends Token(contents)
case class SymToken(override val contents: String) extends Token(contents)
case object LeftParenToken extends Token("(")
case object RightParenToken extends Token(")")
case object DotToken extends Token(".")
case object TrueToken extends Token("#t")
case object FalseToken extends Token("#f")

class Parser {
  type Rule = (Regex, String => Option[Token])
  type TokenizeResult = Either[String, List[Token]]
  type ParseResult = Either[String, (SExpr, List[Token])]

  def parse(source: String): Either[String, SExpr] = {
    tokenize(source) match {
      case Left(msg) => Left(msg)
      case Right(tokens) => {
        parseSExpr(tokens) match {
          case Left(msg) => Left(msg)
          case Right((sexpr, token::_)) => Left(s"expect EOS, but given ${token.contents}")
          case Right((sexpr, Nil)) => Right(sexpr)
        }
      }
    }
  }

  def tokenize(source: String): TokenizeResult = {
    val rules = List[Rule](
      ("[ \t\n]".r, (s) => None),
      ("[-+]?[0-9]+".r, (s) => Some(NumToken(s))),
      ("[-+*/a-zA-Z?]+".r, (s) => Some(SymToken(s))),
      ("[(]".r, (s) => Some(LeftParenToken)),
      ("[)]".r, (s) => Some(RightParenToken)),
      ("[.]".r, (s) => Some(DotToken)),
      ("#t".r, (s) => Some(TrueToken)),
      ("#f".r, (s) => Some(FalseToken))
    )
    tokenize2(source, rules, Nil)
  }

  def tokenize2(source: String, rules: List[Rule], buf: List[Token]): TokenizeResult = {
    if (source.isEmpty) {
      Right(buf.reverse)
    } else {
      tokenize3(source, rules, buf) match {
        case Left(msg) => Left(msg)
        case Right((restString, buf)) => tokenize2(restString, rules, buf)
      }
    }
  }

  def tokenize3(source: String, rules: List[Rule], buf: List[Token]): Either[String, (String, List[Token])] = {
    rules match {
      case Nil => {
        Left(s"unreconized charactor: '${source.head}'")
      }
      case (pattern, converter)::restRules => {
        pattern.findPrefixOf(source) match {
          case None => tokenize3(source, restRules, buf)
          case Some(matched) => {
            val restString = source.drop(matched.length)
            converter(matched) match {
              case Some(token) => Right((restString, token::buf))
              case None => Right((restString, buf))
            }
          }
        }
      }
    }
  }

  def parseSExpr(tokens: List[Token]): ParseResult = {
    tokens match {
      case Nil => Left("expect '(' or atom, but EOS given")
      case LeftParenToken::rest => parseCons(rest)
      case _ => parseAtom(tokens)
    }
  }

  def parseAtom(tokens: List[Token]): ParseResult = {
    tokens match {
      case Nil => Left("expect atom, but EOS given")
      case NumToken(c)::rest => Right(Num(c.toInt) -> rest)
      case SymToken(c)::rest => Right(Sym(c) -> rest)
      case TrueToken::rest => Right(True -> rest)
      case FalseToken::rest => Right(False -> rest)
      case unexpected::_ => Left(s"expect atom, but '${unexpected.contents}' given")
    }
  }

  def parseCons(tokens: List[Token]): ParseResult = {
    tokens match {
      case RightParenToken::rest => Right(NilVal -> rest)
      case _ => {
        parseSExpr(tokens) match {
          case Left(msg) => Left(msg)
          case Right((car, tail)) => {
            parseConsTail(tail) match {
              case Left(msg) => Left(msg)
              case Right((cdr, rest)) => Right(Pair(car, cdr) -> rest)
            }
          }
        }
      }
    }
  }

  def parseConsTail(tokens: List[Token]): ParseResult = {
    tokens match {
      case Nil => Left("expect sexpr, dot, or ), but EOS given")
      case RightParenToken::rest => Right(NilVal -> rest)
      case DotToken::afterDot => {
        parseSExpr(afterDot) match {
          case Left(msg) => Left(msg)
          case Right((sexpr, RightParenToken::rest)) => Right(sexpr -> rest)
          case Right((_, _)) => Left("expect ), but EOS given")
        }
      }
      case _ => {
        parseSExpr(tokens) match {
          case Left(msg) => Left(msg)
          case Right((car, rest)) => {
            parseConsTail(rest) match {
              case Left(msg) => Left(msg)
              case Right((cdr, rest)) => Right(Pair(car, cdr) -> rest)
            }
          }
        }
      }
    }
  }
}
