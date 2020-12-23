;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  RECURSION                    ;;
;;             (with call-by-value)              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (mult <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
         | (rec <id> <expr> <expr>)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (mult l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg)
  (rec id named-expr body))



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list '*  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
           | (list 'rec (list <sym> <s-expr>) <s-expr>) 
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mult (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]
    [(list 'rec (list x e) b) #:when (symbol? x)
         (rec x (parse e) (parse b))]))




;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; box-extend-env :: Symbol Box(Value) Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;        |  (aEnv <id> <value> <env>)
;;        |  (aBoxEnv <id> (box <value>) <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env)
  (aBoxEnv id bval env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)

(define box-extend-env aBoxEnv)


(define (env-lookup x env)
  (match env
    [(mtEnv)
       (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]
    [(aBoxEnv id bval rest) (if (symbol=? id x)
                            (unbox bval)
                            (env-lookup x rest))]))


;; values of expressions
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric values
(define (num+ n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (+ v1 v2)))

(define (num- n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (- v1 v2)))

(define (num* n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (* v1 v2)))

(define (num-zero? n)
  (def (numV v) n) (zero? v))



;; eval :: Expr Env -> Value
;; evaluates an expression in a given environment 
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (num+  (eval l env) (eval r env))]
    [(sub l r) (num-  (eval l env) (eval r env))]
    [(mult l r) (num* (eval l env) (eval r env))]
    [(if0 c t f) (if (num-zero? (eval c env))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (closureV the-arg the-body the-clos-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-clos-env))
               (eval the-body the-ext-env)]
    [(rec id fun-expr body)
                 (def the-ext-env (cyclic-env id fun-expr env))
                 (eval body the-ext-env)]))


;; cyclic-env :: id expr env -> env
;; Assumption: expr is a function expression
(define (cyclic-env id fun-expr env)
  (def fun-val-holder (box 'dummy))
  (def new-env (box-extend-env id fun-val-holder env))
  (def fun-val (eval fun-expr new-env))
  (begin
    (set-box! fun-val-holder fun-val)
    new-env))


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (eval (parse prog) empty-env))


;; some testing
(print-only-errors #t)
(define expr1 '(with (f (fun (y) y)) (f 4)))
(test (run expr1) (numV 4))
(define expr2 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (f 4))))
(test (run expr2) (numV 7))
(define expr3 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (with (x 5) (+ x (f 4))))))
(test (run expr3) (numV 12))
(define expr4 '(rec (fact  (fun (n)
                                (if0 n 1 (* n (fact (- n 1))))))
         (fact 6)))
(test (run expr4) (numV 720))