import org.scalatest.FunSpec
import org.scalatest.Matchers._
import com.autopp.lisp._

class LispTest extends FunSpec {
  val lisp = new Lisp

  describe("Lisp#eval") {
    describe("with (quote x)") {
      it("returns 'x") {
        lisp.eval("(quote x)") should be(Right(Sym("x")))
      }
    }

    describe("with '(1 2)") {
      it("returns (1 2)") {
        lisp.eval("'(1 2)") should be(Right(Pair(Num(1), Pair(Num(2), NilVal))))
      }
    }

    describe("with (if #t 1 2)") {
      it("returns 1") {
        lisp.eval("(if #t 1 2)") should be(Right(Num(1)))
      }
    }

    describe("with (if #f 1 2)") {
      it("returns 3") {
        lisp.eval("(if #f 1 2)") should be(Right(Num(2)))
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

    describe("with (define f 42) then f") {
      it("returns 42") {
        lisp.eval("(define f 42)")
        lisp.eval("f") should be(Right(Num(42)))
      }
    }

    describe("with (define n 10) and (define (f x) (+ n x)) and then (f 32)") {
      it("returns 42") {
        lisp.eval("(define n 10)")
        lisp.eval("(define (f x) (+ n x))")
        lisp.eval("(f 32)") should be(Right(Num(42)))
      }
    }

    describe("(let ((x 40) (y 2)) (+ x y))") {
      it("returns 42") {
        lisp.eval("(let ((x 40) (y 2)) (+ x y))") should be(Right(Num(42)))
      }
    }

    describe("(let ((x 44)) (let ((y 2)) (- x y)))") {
      it("returns 42") {
        lisp.eval("(let ((x 44)) (let ((y 2)) (- x y)))") should be(Right(Num(42)))
      }
    }

    describe("((let ((x 30)) (let ((y 10)) (lambda (z) (+ x y z)))) 2)") {
      it("returns 42") {
        lisp.eval("((let ((x 30)) (let ((y 10)) (lambda (z) (+ x y z)))) 2)") should be(Right(Num(42)))
      }
    }

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

    describe("with (car (cons #t #f))") {
      it("returns #t") {
        lisp.eval("(car (cons #t #f))") should be(Right(True))
      }
    }

    describe("with (cdr (cons #t #f))") {
      it("returns #f") {
        lisp.eval("(cdr (cons #t #f))") should be(Right(False))
      }
    }

    describe("with (atom 1)") {
      it("returns true") {
        lisp.eval("(atom 1)") should be(Right(True))
      }
    }

    describe("with (atom ())") {
      it("returns true") {
        lisp.eval("(atom ())") should be(Right(True))
      }
    }

    describe("with (atom (quote (1 . 2)))") {
      it("returns false") {
        lisp.eval("(atom (quote (1 . 2)))") should be(Right(False))
      }
    }

    describe("with (atom (quote (1 2 3)))") {
      it("returns false") {
        lisp.eval("(atom (quote (1 2 3)))") should be(Right(False))
      }
    }

    describe("with (= (+ 1 2) (- 5 2) 3)") {
      it("returns #t") {
        lisp.eval("(= (+ 1 2) (- 5 2) 3)") should be(Right(True))
      }
    }

    describe("with (= (+ 1 2) (+ 5 2) 3)") {
      it("returns #t") {
        lisp.eval("(= (+ 1 2) (+ 5 2) 3)") should be(Right(False))
      }
    }
  }
}
