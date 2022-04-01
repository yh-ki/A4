#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86)

;; Registers used
(define rax 'rax)
(define rbx 'rbx) ; tmp used for type tag tests
(define r8  'r8)  ; scratch
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; argument for C call

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        (Sub rsp 8)
        (Jmp 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)         (compile-value i)]
    [(Bool b)        (compile-value b)]
    [(Char c)        (compile-value c)]
    [(Eof)           (compile-value eof)]
    [(Var x)         (compile-variable x c)]
    [(Prim0 p)       (compile-prim0 p c)]
    [(Prim1 p e)     (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitive +
    [(PrimN p es)    (compile-primN p es c)]
    [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)   (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let (list x) (list e1) e2)
     (compile-let1 x e1 e2 c)]
    ;; TODO: implement let, let*, case, cond
    [(Let xs es e)   (compile-let xs es e c)]
    [(Let* xs es e)  (seq)]
    [(Case ev cs el) (c ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))

(define (compile-let xs1 xs2 es e c)
  (match es
    ['() (seq (compile-e e (cons xs2 c))]
    [(cons el es)
     (match xs1
       [(cons x xs)
        (seq (compile-e el c)
             (Push rax)
             (compile-let xs xs2 es e c))])]))
  

;;[Listof CondClause] Expr -> Asm
(define (compile-cond cs el c)
  (match cs
    ['() (seq  (compile-e el c))]
    [(list (Clause e1 e2) x ...)
     (let ((c1 (gensym 'cond))
           (c2 (gensym 'cond)))
     (seq (compile-e e1 c)
          (Cmp rax val-false)
          (Je c1)
          (compile-e e2 c)
          (Jmp c2)
          (Label c1)
          (compile-cond x el c)
          (Label c2)))]))

;;Expr [Listof CaseClause] Expr -> Asm
(define (compile-case ev cs el c)
  (match cs
    ['() (seq  (compile-e el c))]
    [(list (Clause a b) x ...)
     (let ((c1 (gensym 'case))
           (c2 (gensym 'case)))
     (seq (compile-e ev c)
          (contain? a)
          (Pop 'rcx)
          (Cmp 'rcx val-true)
          (Jne c1)
          (compile-e b c)
          (Jmp c2)
          (Label c1)
          (compile-case ev x el c)
          (Label c2)))]))

(define (contain? a)
  (match a
    ['() (seq (Mov 'rcx val-false)
              (Push 'rcx))]
    [(list x y ...)
     (let ((d1 (gensym 'cont))
           (d2 (gensym 'cont)))
       (seq (Cmp rax (value->bits x))
            (Jne d1)
            (Mov 'rcx val-true)
            (Push 'rcx)
            (Jmp d2)
            (Label d1)
            (contain? y)
            (Label d2)))]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p c)))

;; Op1 CEnv -> Asm
(define (compile-op1 p c)
  (match p
    ['add1
     (seq (assert-integer rax c)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax c)
          (Sub rax (value->bits 1)))]
    ['zero?
     (let ((l1 (gensym 'is_zero)))
       (seq (assert-integer rax c)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char?
     (let ((l1 (gensym 'is_char)))
       (seq (And rax mask-char)
            (Xor rax type-char)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char->integer
     (seq (assert-char rax c)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint c)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (let ((l1 (gensym 'is_eof)))
       (seq (Cmp rax val-eof)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['write-byte
     (seq (assert-byte c)
          (pad-stack c)
          (Mov rdi rax)
          (Call 'write_byte)
          (unpad-stack c)
          (Mov rax val-void))]

    ;; TODO: implement -, abs, integer?, boolean?, etc.
    ['-
     (seq (assert-integer rax c)
          (Mov 'r9 rax)
          (Mov rax 0)
          (Sub rax 'r9))]
    ['abs
     (let ((ab (gensym 'a)))
       (seq (assert-integer rax c)
            (Cmp rax 0)
            (Jg ab)
            (Mov 'r8 rax)
            (Mov rax 0)
            (Sub rax 'r8)
            (Label ab)))]
    ['not
     (let ((t (gensym 'no)))
       (seq (Cmp rax val-false)
            (Je t)
            (Mov rax val-false)
            (Jmp 'not_done)
            (Label t)
            (Mov rax val-true)
            (Label 'not_done)))]
    ['integer?
     (let ((i1 (gensym 'in))
           (i2 (gensym 'in)))
       (seq (And rax 1)
            (Cmp rax 0)
            (Jne i1)
            (Mov rax val-true)
            (Jmp i2)
            (Label i1)
            (Mov rax val-false)
            (Label i2)))]
    ['boolean?
     (let ((b1 (gensym 'bo))
           (b2 (gensym 'bo))
           (b3 (gensym 'bo)))
       (seq (Cmp rax val-true)
            (Je b1)
            (Cmp rax val-false)
            (Je b1)
            (Jmp b2)
            (Label b1)
            (Mov rax val-true)
            (Jmp b3)
            (Label b2)
            (Mov rax val-false)
            (Label b3)))]))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p c)))

(define (compile-primN p es c)
  (seq (compile-e* es c)
       (compile-opN p es c)))

(define (compile-opN p es c)
  (match p
    ['+
     (let ((n1 (gensym))
           (n2 (gensym)))
         (seq (Mov rax 0)
              (Mov 'r9 (length es))
              (Label n1)
              (Cmp 'r9 0)
              (Je n2)
              (Sub 'r9 1)
              (Pop r8)
              (assert-integer r8 c)
              (Add rax r8)
              (Jmp n1)
              (Label n2)))]))

;; Op2 CEnv -> Asm
(define (compile-op2 p c)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Sub r8 rax)
          (Mov rax r8))]))



;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))







;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))



;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (match (even? (length c))
    [#t (seq (Sub rsp 8))]
    [#f (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (match (even? (length c))
    [#t (seq (Add rsp 8))]
    [#f (seq)]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (assert-type mask type)
  (λ (arg c)
    (seq (Mov rbx arg)
         (And rbx mask)
         (Cmp rbx type)
         (Jne (error-label c)))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (value->bits 0))
         (Jl (error-label c))
         (Cmp rax (value->bits 1114111))
         (Jg (error-label c))
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp (error-label c))
         (Label ok))))

(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (value->bits 0))
       (Jl (error-label c))
       (Cmp rax (value->bits 255))
       (Jg (error-label c))))

;; CEnv -> Label
;; Determine correct error handler label to jump to.
(define (error-label c)
  (match (even? (length c))
    [#t 'raise_error]
    [#f 'raise_error_align]))
