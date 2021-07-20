#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Q1
;; int int int -> int_list
;; produces list of integers between first and second int with third int interval

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Q2
;; string_list string -> string_list
;; produces a list of string where string is added to each string in the list

(define (string-append-map stringlist suffix)
  (if (null? stringlist)
      null
      (cons (string-append (car stringlist) suffix) (string-append-map (cdr stringlist) suffix))))

;; Q3
;; list int -> int or error
;; produces the ith number of a list or error where
;; i the remainder produced when dividing n by thelist’s length

(define (list-nth-mod list1 n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? list1) (error "list-nth-mod: empty list")]
        [#t (car (list-tail list1 (remainder n (length list1))))]))
            
;; Q4
;; stream int -> list
;; produces a list of the first n numbers produces by a stream

(define (stream-for-n-steps stream n)
  (letrec ([f (lambda (stream n ans)
                (let ([pr (stream)])
                  (if (= n 0)
                      (reverse ans)
                      (f (cdr pr) (- n 1) (cons (car pr) ans)))))])
    (f stream n null)))

;; Q5
;; -> stream
;; produces a stream that is like the stream of natural numbers except numbers divisble by 5 are negated

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (modulo x 5))
                    (cons (* x -1) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; Q6
;; -> stream
;; produces a stream dan-then-dog, where the elements of the stream alternate
;; between the strings "dan.jpg" and "dog.jpg"

(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= 1 (modulo x 2))
                    (cons "dan.jpg" (lambda () (f (+ x 1))))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))
  
;; Q7
;; stream -> stream
;; produces the pair (0 . v) if stream produces v for its ith element

(define (stream-add-zero stream)
  (lambda ()
    (let ([pr (stream)])
      (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

;; Q8
;; list list -> stream
;; produces pairs where the first part is from first list and second part from second list
;; lists are non-empty and can be of different length

(define (cycle-lists lista listb)
  (letrec ([f (lambda (x)
                (cons
                 (cons (list-nth-mod lista x) (list-nth-mod listb x))
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;; Q9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([ith (vector-ref vec n)])
                      (if (and (pair? ith) (equal? (car ith) v))
                          ith
                          (f (+ n 1))))))])
    (f 0)))

;; Q10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([new-ans (assoc v xs)])
            (and new-ans
                 (begin
                   (vector-set! memo pos new-ans)
                   (set! pos (remainder (+ pos 1) n))
                   new-ans)))))))
  

  


