;; Programming Languages, Homework 5

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;;a
;; 'a list -> MUPL list
;; produces a MUPL list from a Racket list

(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [(apair (car xs) (racketlist->mupllist (cdr xs)))]))

;;b
;; MUPL list -> ' a list
;; produces a racket list from a MUPL list
(define (mupllist->racketlist xs)
  (cond [(aunit? xs) null]
        [(cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)]
               [s (mlet-var e)])
           (if (string? s)
               (eval-under-env (mlet-body e) (cons (cons s v) env))
               (error "MUPL mlet applied to non-string")))]
        [(call? e)
         (letrec ([v1 (eval-under-env (call-funexp e) env)]
                  [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (eval-under-env (fun-body (closure-fun v1))
                               (let ([ext1 (cons (fun-nameopt (closure-fun v1)) v1)]
                                     [ext2 (cons (fun-formal (closure-fun v1)) v2)])
                               (if (fun-nameopt (closure-fun v1))
                                   (cons ext1 (cons ext2 (closure-env v1)))
                                   (cons ext2 (closure-env v1)))))
               (error "MUPL call applied to non-closure")))]
        [(closure? e) e]
        [(apair? e) (apair
                     (eval-under-env (apair-e1 e) env)
                     (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
               (if (apair? v1)
                   (apair-e1 v1)
                   (error "You're trying to apply it to sth that's not a pair")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
               (if (apair? v1)
                   (apair-e2 v1)
                   (error "You're trying to apply it to sth that's not a pair")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (equal? v1 (aunit))
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [(if (equal? (car (car lstlst)) (var-string e2))
             (cdr (car lstlst))
             (mlet* (cdr lstlst) e2))]))


(define (ifeq e1 e2 e3 e4)
  (let ([_x (eval-exp e1)]
        [_y (eval-exp e2)])
    (ifgreater _x _y e4 (ifgreater _y _x e4 e3))))

;; Problem 4

(define mupl-map (fun #f "fun"
                      (fun "map" "list"
                           (ifaunit (var "list")
                                    (aunit)
                                    (apair (call (var "fun") (fst (var "list")))
                                           (call (var "map") (snd (var "list"))))))))                                

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
