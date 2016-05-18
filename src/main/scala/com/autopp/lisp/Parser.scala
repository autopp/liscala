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
case object QuoteToken extends Token("'")

class Parser {
  type Rule = (Regex, String => Option[Token])
  type MayError[A] = Lisp.MayError[A]
  type TokenizeResult = MayError[List[Token]]
  type ParseResult = MayError[(SExpr, List[Token])]

  def parse(source: String): MayError[SExpr] = {
    tokenize(source).right.flatMap { tokens =>
      parseSExpr(tokens).right.flatMap {
        case (sexpr, token::_) => Left(s"expect EOS, but given ${token.contents}")
        case (sexpr, Nil) => Right(sexpr)
      }
    }
  }

  def tokenize(source: String): TokenizeResult = {
    val rules = List[Rule](
      ("[ \t\n]".r, (s) => None),
      ("[-+]?[0-9]+".r, (s) => Some(NumToken(s))),
      ("[-+*/a-zA-Z?!=]+".r, (s) => Some(SymToken(s))),
      ("[(]".r, (s) => Some(LeftParenToken)),
      ("[)]".r, (s) => Some(RightParenToken)),
      ("[.]".r, (s) => Some(DotToken)),
      ("#t".r, (s) => Some(TrueToken)),
      ("#f".r, (s) => Some(FalseToken)),
      ("'".r, (s) => Some(QuoteToken))
    )
    tokenize2(source, rules, Nil)
  }

  def tokenize2(source: String, rules: List[Rule], buf: List[Token]): TokenizeResult = {
    if (source.isEmpty) {
      Right(buf.reverse)
    } else {
      tokenize3(source, rules, buf).right.flatMap {
        case (restString, buf) => tokenize2(restString, rules, buf)
      }
    }
  }

  def tokenize3(source: String, rules: List[Rule], buf: List[Token]): MayError[(String, List[Token])] = {
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
              case None => Right((restString, buf))
              case Some(token) => Right((restString, token::buf))
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
      case QuoteToken::rest => {
        parseSExpr(rest).right.map {
          case (sexpr, rest) => (Pair(Sym("quote"), Pair(sexpr, NilVal)), rest)
        }
      }
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
        parseSExpr(tokens).right.flatMap {
          case (car, tail) => parseConsTail(tail).right.map { case (cdr, rest) => (Pair(car, cdr), rest) }
        }
      }
    }
  }

  def parseConsTail(tokens: List[Token]): ParseResult = {
    tokens match {
      case Nil => Left("expect sexpr, dot, or ), but EOS given")
      case RightParenToken::rest => Right(NilVal -> rest)
      case DotToken::afterDot => parseSExpr(afterDot).right.flatMap {
        case (sexpr, RightParenToken::rest) => Right(sexpr -> rest)
        case _ => Left("expect ), but EOS given")
      }
      case _ => {
        parseSExpr(tokens).right.flatMap {
          case (car, rest) => {
            parseConsTail(rest).right.map {
              case (cdr, rest) => (Pair(car, cdr), rest)
            }
          }
        }
      }
    }
  }
}
