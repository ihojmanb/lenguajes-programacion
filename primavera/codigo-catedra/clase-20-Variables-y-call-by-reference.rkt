;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                VARIABLES 
;;  - Call-by-value and call-by-refernce
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang play


#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (refun (<sym>) <expr>)  ; call by ref
         | (app <expr> <expr>)
         | (set <id> <expr>)
         | (seqn <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (refun arg body)
  (app f-name f-arg)
  (set id val-expr)
  (seqn expr1 expr2)
  (seqnn exprs))



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list 'refun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
           | (list 'set <sym> <s-expr>)
           | (list 'seqn <s-expr> <s-expr>)
           | (list <s-expr> <s-expr>)
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list 'refun (list x) b) (refun x (parse b))]
    [(list 'with (list x e) b) #:when (symbol? x)
                               (app (fun x (parse b)) (parse e))]
    [(list 'set id e) (set id (parse e))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'seqnn e ...) (seqnn (map parse e))]
    [(list f a) (app (parse f) (parse a))]))


;; Environment abstract data type

;; empty-env  :: Env
;; extend-env :: Symbol Loc Env -> Env
;; lookup-env :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <loc> <env>)
(deftype Env
  (mtEnv)
  (aEnv id loc env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (lookup-env x env)
  (match env
    [(mtEnv) (error 'lookup-env "free identifier: ~a" x)]
    [(aEnv id loc rest) (if (symbol=? id x)
                            loc
                            (lookup-env x rest))]))



;; Store abstract data type

;; empty-sto  :: Store
;; extend-sto :: Loc Value Store -> Store
;; lookup-sto :: Loc Store -> Value

;; Implementation of the ADT

;; <sto> ::= mtSto
;;         | (aSto <loc> <value> <sto>)
(deftype Store
  (mtSto)
  (aSto loc val sto))

(define empty-sto (mtSto))
 
(define extend-sto aSto)

(define (lookup-sto x sto)
  (match sto
    [(mtSto) (error 'lookup-sto "No value at location ~a" x)]
    [(aSto loc val rest) (if (equal? loc x)
                             val
                             (lookup-sto x rest))]))

;; next-location :: Store -> Loc
;; returns next free location of a store
(define (next-location sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ rest) (add1 (next-location rest))]))


;; values of expressions
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>)
;;          |  (refclosureV <sym> <s-expr> <env>)
(deftype Value
  (numV n)
  (closureV id body env)
  (refclosureV id body env))


;; Pair of values and stores
;; <value*store> ::= (v*s <val> <sto>)
(deftype Val*Store
  (v*s val sto))


;; Auxiliary functions handling numeric values
(define (num+ n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (+ v1 v2)))

(define (num- n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (- v1 v2)))

(define (num-zero? n)
  (def (numV v) n) (zero? v))


;; interp :: Expr Env Store -> Value*Store
(define (interp expr env sto)
  (match expr

    [(num n) (v*s (numV n) sto)]
    
    [(id x) (v*s (lookup-sto (lookup-env x env) sto) sto)]
    
    [(fun id body) (v*s (closureV id body env) sto)]

    [(refun id body) (v*s (refclosureV id body env) sto)]
    

    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto))
     (if (num-zero? c-val)
         (interp t env c-sto)
         (interp f env c-sto))]
    
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num+ l-val r-val) r-sto)]

    [(sub l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num- l-val r-val) r-sto)]
 
    [(app fun-expr arg-expr)
     (def (v*s fun-val fun-sto) (interp fun-expr env sto))
     (match fun-val
       [(closureV id body fenv)
        (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto))
        (def new-loc (next-location arg-sto))
        (interp body
                (extend-env id new-loc fenv)
                (extend-sto new-loc arg-val arg-sto))]
       
       [(refclosureV id body fenv) 
        (def loc (lookup-env (id-x arg-expr) env))
        (interp body
                (extend-env id loc fenv)
                fun-sto)])]
     
    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto))
     (interp expr2 env sto1)]



    
    [(set id val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (def loc (lookup-env id env))
     (v*s val-val
          (extend-sto loc val-val val-sto))]))


;; run :: s-expr -> value
(define (run prog)
  (def (v*s v _) (interp (parse prog) empty-env empty-sto))
  v)



;; some testing
(print-only-errors #t)

(define expr1 '(with (b 0)
                     (if0 (seqn (set b 5) b)
                          1
                          b)))
(test (run expr1) (numV 5))


(define expr2 '(with (b 0)
                     (seqn (set b (+ 1 b))
                           b)))
(test (run expr2) (numV 1))

(define expr3 '(with (a 1)
                     (with (f (fun (x) (+ x a)))
                           (seqn (set a 2) (f 5)))))
(test (run expr3) (numV 7))

(define expr999 '(with (a 1)
       (with (f (fun (x) (+ x a)))
         (seqnn ((set a 2) (f 5))))))
(test (run expr999) (numV 7))



(define expr4 '(with (v 0)
                     (with (f (fun (y) (set y 5)))
                           (seqn (f v) v))))
(test (run expr4) (numV 0))

(define expr5 '(with (v 0)
                     (with (f (refun (y) (set y 5)))
                           (seqn (f v) v))))
(test (run expr5) (numV 5))
