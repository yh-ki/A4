#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type REnv = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive +
    [(PrimN p es)
     (match (interp*-env es r)
       ['err 'err]
       [vs (interp-primN p vs)])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (interp-cond cs e r)]
    ;; TODO: implement case
    [(Case ev cs el) (interp-case ev cs el r)]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let (list x) (list e1) e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]
    ;; TODO: implement let, let*
    [(Let  xs es e)
     (match (interp*-env es r)
       ['err 'err]
       ['() (interp-env e r)]
       [vs (interp-env e (exts r xs vs))])]
    [(Let* xs es e)(interp-let* xs es e r)]))


;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    ['() 'err]
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

(define (exts r xs vs)
  (match xs
    [(cons x '())
     (match vs
       [(cons v '())
        (let ((y (assoc x r))) (if y (cons (list x v) (remove y r)) (cons (list x v) r)))])]
    [(cons x xs)
     (match vs
       [(cons v vs)
        (let ((y (assoc x r))) (if y (cons (exts r xs vs) (cons (list x v) (remove y r))) ((exts r xs vs) (cons (list x v) r))))])]))

(define (interp-let* xs es e r)
  (match es
    ['() (interp-env e r)]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match xs
            [(cons x xs) (interp-let* xs es e (cons (list x v) r))])])]))

(define (interp-cond cs e r)
  (match cs
       ['() (interp-env e r)]
       [(list (Clause e1 e2) x ...) (if (interp-env e1 r) (interp-env e2 r) (interp-cond x e r))]))

(define (interp-case e cs el r)
  (match cs
    ['() (interp-env el r)]
    [(list (Clause a b) x ...) (if (member (interp-env e r) a) (interp-env b r) (interp-case e x el r))]))

(define (interp-es es)
  (match es
    ['() '()]
    [(list v x ...) (cons (interp-env v) (interp-es x))]))

