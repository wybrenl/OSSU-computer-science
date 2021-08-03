#lang racket

(provide (all-defined-out))

(struct card (suit rank) #:transparent)

(define card1 (card "hearts" 10))

(card? card1)

(card-suit card1)
(card-rank card1)


;; Racket is meta-language

;; below is forming a little programming language - the arithmetic language
(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct eq-num (e1 e2) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)
(struct closure (env fun) #:transparent)

;; below in the language implementation through an interpreter, eval-exp is an interpreter
(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (const (- (const-int
                                (eval-exp (negate-e e)))))]
        [(add? e)
         (let ([v1 (const-int (eval-exp (add-e1 e)))]
              [v2 (const-int (eval-exp (add-e2 e)))])
           (const (+ v1 v2)))]
        [(multiply? e)
          (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
               [v2 (const-int (eval-exp (multiply-e2 e)))])
            (const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))
                    