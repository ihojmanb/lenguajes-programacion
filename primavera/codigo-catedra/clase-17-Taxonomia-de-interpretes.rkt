;;  Degradee of semantic interpreters for our
;;  arithmetical language (static scope, call-by-value)
;;  Functional representation of environments


#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg))


;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]))


;; Procedural representation of environments 

(define empty-env
  (λ (id) (error "free identifier: ~a" id)))

(define (env-lookup id env)
  (env id))

(define (extend-env new-id value env)
  (λ (id)
    (if (symbol=? id new-id)
        value
        (env id))))



;;; Auxiliary functions handling numeric values
#;(define (num+ n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (+ v1 v2)))
;
#;(define (num-zero? n)
  (def (numV v) n) (zero? v))
;
;
;
;
;;;;;;;;;;    1st Interpreter    ;;;;;;;;;
;;; It uses Racket's numbers to represent interpreted language's
;;; numbers, but doesn't use Racket functions (closures)
;;; to represent the interpreted language's functions (closures)
;
;;; values of expressions
#;(deftype Value
  (numV n)
  (closureV id body env))
;
;;; eval :: Expr Env -> Value
#;(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]
    [(id x) (env-lookup x env)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(if0 c t f) (if (num-zero? (eval c env)) (eval t env) (eval f env))]))
;
;
;
;
;;;;;;;;;;    2nd Interpreter    ;;;;;;;;;
;;; It uses Racket's numbers and functions to represent
;;; the interpreted language's numbers and functions, and
;;; wraps them into datatype Value
;
;;; values of expressions
#;(deftype Value
  (numV n)
  (closureV f))
;
;;; eval :: Expr Env -> Value
#;(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (def fun-val (λ (arg-val) (eval body (extend-env id arg-val env))))
                   (closureV fun-val)]
    [(app f e) (def (closureV fun-val) (eval f env))
               (def arg-val (eval e env))
               (fun-val arg-val)]    
    [(id x) (env-lookup x env)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(if0 c t f) (if (num-zero? (eval c env)) (eval t env) (eval f env))]))




;;;;;;;;;    3rd Interpreter    ;;;;;;;;;
;; Meta-circular interpreter

;; eval :: Expr Env -> number/procedure
(define (eval expr env)
  (match expr
    [(num n) n]
    [(fun id body) (λ (arg-val) (eval body (extend-env id arg-val env)))]
    [(app f e) ((eval f env) (eval e env))]
    [(id x) (env-lookup x env)]
    [(add l r) (+ (eval l env) (eval r env))]
    [(if0 c t f) (if (zero? (eval c env)) (eval t env) (eval f env))]))


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (eval (parse prog) empty-env))


;; Some testing (for the 3rd interpreter)
(print-only-errors #t)
(define expr1 '(with (f (fun (y) y)) (f 4)))
(test (run expr1) 4)
(define expr2 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (f 4))))
(test (run expr2) 7)
(define expr3 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (with (x 5) (+ x (f 4))))))
(test (run expr3) 12)


;;; Some testing (for the 1st and 2nd interpreters)
;(print-only-errors #t)
;(define expr1 '(with (f (fun (y) y)) (f 4)))
;(test (run expr1) (numV 4))
;(define expr2 '(with (x 3)
;                       (with (f (fun (y) (+ x y)))
;                             (f 4))))
;(test (run expr2) (numV 7))
;(define expr3 '(with (x 3)
;                       (with (f (fun (y) (+ x y)))
;                             (with (x 5) (+ x (f 4))))))
;(test (run expr3) (numV 12))