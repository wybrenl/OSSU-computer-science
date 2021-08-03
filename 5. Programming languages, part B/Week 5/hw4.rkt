
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;;Q1
;; int * int * int -> int list
;; produces a list of numbers from low to high (including low and possibly high) seperated by stride and in sorted order

              
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;;Q2
;; string list * string -> string list
;; produces a string list with the suffix appended to each string in the original string list


(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


;;Q3
;; 'a list * int -> 'a
;; produces error for negative number and empty list, else returns ith element from the list

(define (list-nth-mod xs n)
  (cond [(< n 0)(error "list-nth-mod: negative number")]
        [(null? xs)(error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;;Q4
;; stream * int -> 'a list
;; produces a list of the first n values of stream s in order

(define (stream-for-n-steps s n)
  (cond [(= n 0) empty]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

;;Q5
;; stream
;; stream of natural numbers except number divisble by 5 are negated

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;;Q6
;; stream
;; stream of ("dan.jpg", thunk) where if the next thunk is called it produces a pair ("dog.jpg", thunk etc)

(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dan.jpg")
                    (cons "dan.jpg" (lambda () (f "dog.jpg")))
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;;Q7
;; stream -> stream
;; 

(define (stream-add-zero s)
  (lambda ()
    (let ([pr (s)])
      (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))


;;Q8
;; 'a list * 'a list -> stream
;; produces a stream of two lists where the elements are pairs from both lists

(define (cycle-lists xs ys)
  (letrec ([x-copy xs]
           [y-copy ys]
           [f (lambda (x y)
                (cond
                 [(and (null? x) (null? y) (cons (cons (car x-copy) (car y-copy)) (lambda () (f (cdr x-copy) (cdr y-copy)))))]
                 [(null? x) (cons (cons (car x-copy) (car y)) (lambda () (f (cdr x-copy) (cdr y))))]
                 [(null? y) (cons (cons (car x) (car y-copy)) (lambda () (f (cdr x) (cdr ys))))]
                 [#t (cons (cons (car x) (car y)) (lambda () (f (cdr x) (cdr y))))]))])                
    (lambda () (f xs ys))))

;;Q9
;; int * vector -> pair
;; produces the pair for which the first elements is the same as the value v, #f otherwise

(define (vector-assoc value vector)
  (letrec
      ([len (vector-length vector)]
       [helper (lambda (vector len pos)
          (cond
            [(equal? pos len) #f]
            [(not (pair? (vector-ref vector pos))) (helper vector len (+ 1 pos))]
            [(equal? (car (vector-ref vector pos)) value) (vector-ref vector pos)]
            [#t (helper vector len (+ 1 pos))]))])
  (helper vector len 0)))

;;Q10
;; 'a list * int -> function

(define (cached-assoc2 xs n)
  (let ([memo (make-vector n #f)]
           [slot 0])
    (lambda (value)
                (let ([ans (vector-assoc value memo)])
                  (if ans
                      (begin (print "took from memo") ans)
                      (let ([ans2 (assoc value xs)])
                        (if ans2
                            (begin (vector-set! memo slot (assoc value xs)) (if (= (+ 1 slot) n) (set! slot 0) (set! slot (add1 slot))) (print "added to memo")
                                   ans2)
                            #f)))))))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [slot 0]
           [helper (lambda (value)
                (let ([ans (vector-assoc value memo)])
                  (begin (print memo) (if ans
                      (begin (print "took from memo") ans)
                      (let ([ans2 (assoc value xs)])
                        (if ans2
                            (begin (vector-set! memo slot (assoc value xs)) (if (= (+ 1 slot) n) (set! slot 0) (set! slot (add1 slot))) (print "added to memo")
                                   ans2)
                            #f))))))])
    helper))
                 
(define friend-list (list (cons "H" 1) (cons "A" 2) (cons "C" 3)))

(define friend-search (cached-assoc friend-list 2))




              
                