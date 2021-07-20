;PLB: week 5
#lang racket
(provide (all-defined-out))

; function calls
(define s "hello")

(define x 3) ; val x = 3 in ml
(define y (+ x 2)) ; + is a function, call it here

(define cube1
  (lambda (x)      ; anonymous function like fn
    (* x (* x x)))) ; body

(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))

(define (pow1 x y) ; x to the yth power (y non-negative)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define three-to-the (pow2 3))

; lists

;null -> emtpy list
;cons -> ::
;car -> head of list
;cdr -> tail of list
;null? check for empty
;(list e1 ...en) -> make list

; sum all the number in a list

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

; parentheses matter

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

; dynamic typing

(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))
(define zs (list #f "hi" 14))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum2 (cdr xs)))
              (sum2 (cdr xs))))))

; cond
; syntactic sugar for if then else expressions

(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

(define (sum4 xs)
  (cons [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? (car xs)) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [#t (sum4 (cdr xs))]))
        

; local bindings

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
              tlans
              (car xs)))]))

; mcons cells are mutable, cons are not
; mcons
; mcar
; mcdr
; mpair?
; set-mcar!
; set-mcdr!

; Thunks enable you to delay a computation 
(define (my-if x y z)
  (if x (y) (z)))

(define (fact2 n)
  (my-if (= n 0)
         (lambda ()1 )
         (lambda() (* n (fact (- n 1))))))

;(define (f th)
 ; (if (..) 0 (... (th) ..)))

; force and delay
(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

;(my-mult x (let ([x (my-delay (lambda () (slow-add 3 4)))])
 ;            (lambda () (my-force p))))

; streams

;1 1 1 1 1 1 1 1...

(define ones (lambda () (cons 1 ones)))

;1 2 3 4 5...

(define (f x) (cons x (lambda () (f (+ x 1)))))

(define nats (lambda () (f 1)))

(define nats1
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 2 4 6 8 16 ...
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

;(define (stream-maker fn arg) ...)

;memoization
(define fibonacci3
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)]) ; assoc check if x is in car of a list of pairs
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))

;macros






