import org.scalatest.FunSpec
import org.scalatest.Matchers._
import com.autopp.lisp._

class LispTest extends FunSpec {
  val lisp = new Lisp

  describe("Lisp#eval") {
    describe("with (+ 41 1)") {
      it("returns 42") {
        lisp.eval("(+ 41 1)") should be(Right(Num(42)))
      }
    }

    describe("with (- 44 2)") {
      it("returns 42") {
        lisp.eval("(- 44 2)") should be(Right(Num(42)))
      }
    }

    describe("with (cons 1 2)") {
      it("returns (1 . 2)") {
        lisp.eval("(cons 1 2)") should be(Right(Pair(Num(1), Num(2))))
      }
    }

    describe("with (quote x)") {
      it("returns 'x") {
        lisp.eval("(quote x)") should be(Right(Sym("x")))
      }
    }

    describe("with (if #t 1 2)") {
      it("returns 1") {
        lisp.eval("(if #t 1 2)") should be(Right(Num(1)))
      }
    }


    describe("with (if #f 1 2)") {
      it("returns 1") {
        lisp.eval("(if #t 1 2)") should be(Right(Num(2)))
      }
    }

    describe("with (if #f 1)") {
      it("returns nil") {
        lisp.eval("(if #f 1)") should be(Right(NilVal))
      }
    }

    describe("with ((lambda (x y) (+ x y)) 41 1)") {
      it("returns 42") {
        lisp.eval("((lambda (x y) (+ x y)) 41 1)") should be(Right(Num(42)))
      }
    }

    describe("with (((lambda (x) (lambda (y) (- x y))) 44) 2)") {
      it("returns 42") {
        lisp.eval("(((lambda (x) (lambda (y) (- x y))) 44) 2)") should be(Right(Num(42)))
      }
    }
  }
}
