#lang racket
(provide (all-defined-out))
;(require "foo.rkt")
(define a (+ 5 5))
(define s "Hello")

(define x 3) ; val x = 3
(define y (+ x 2)); + is a function

(define cube1
  (lambda (x)
    (* x (* x x)))) ;

(define cube2
  (lambda (x)
    (* x x x)));

(define (cube3 x)
  (* x x x))

(define (my-if x y z)
  (if x (y) (z)))

(define (fact n)
  (my-if (= n 0)
         (lambda () 1)
         (lambda () (* n (fact (- n 1))))))


(define ones (lambda () (cons 1 ones)))

(define (f x) (cons x (lambda () f (+ x 1))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
  (lambda () (f arg))))

(define nats (stream-maker + 1))