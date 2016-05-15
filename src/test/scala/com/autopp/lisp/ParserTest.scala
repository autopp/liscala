import org.scalatest.FunSpec
import org.scalatest.Matchers._
import com.autopp.lisp._

class ParerTest extends FunSpec {
  val parser = new Parser

  describe("Parser") {
    describe("#tokenize") {
      describe("with (+ 41 1)") {
        it("returns tokens") {
          val expected = List(
            LeftParenToken, SymToken("+"), NumToken("41"), NumToken("1"), RightParenToken
          )
          parser.tokenize("(+ 41 1)") should be(Right(expected))
        }
      }

      describe("with (#t . #f)") {
        it("returns tokens") {
          val expected = List(
            LeftParenToken, TrueToken, DotToken, FalseToken, RightParenToken
          )
          parser.tokenize("(#t . #f)") should be(Right(expected))
        }
      }
    }

    describe("#parse") {
      describe("with (+ 41 1)") {
        it("returns list") {
          val expected = Pair(Sym("+"), Pair(Num(41), Pair(Num(1), NilVal)))
          parser.parse("(+ 41 1)") should be(Right(expected))
        }
      }

      describe("with (#t  () . #f)") {
        it("returns pair") {
          val expected = Pair(True, Pair(NilVal, False))
          parser.parse("(#t () . #f)") should be(Right(expected))
        }
      }

      describe("with foo") {
        it("returns symbol") {
          parser.parse("foo") should be(Right(Sym("foo")))
        }
      }
    }
  }
}
