;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                MUTATION
;;  - static scope
;;  - call-by-name (??? I see a call-by-value implementation. Probably a typo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
         | (newbox <expr>)
         | (openbox <expr>)
         | (setbox <expr> <expr>)
         | (seqn <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg)
  (newbox val-expr)
  (openbox box-expr)
  (setbox box-expr val-expr)
  (seqn expr1 expr2))



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
           | (list 'newbox <s-expr>)
           | (list 'openbox <s-expr>)
           | (list 'setbox <s-expr> <s-expr>)
           | (list 'seqn <s-expr> <s-expr>)
           | (list <s-expr> <s-expr>)
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
;; (Note that the application clause should be
;; after the newbox and openbox clauses)
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]
    [(list 'newbox e) (newbox (parse e))]
    [(list 'openbox e) (openbox (parse e))]
    [(list 'setbox e1 e2) (setbox (parse e1) (parse e2))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
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
;;          |  (boxV <loc>) 
(deftype Value
  (numV n)
  (closureV id body env)
  (boxV loc))


;; Pair of values and stores
;; <value*store> ::= (v*s <val> <sto>)
(deftype Val*Store
  (v*s val sto))


;; Auxiliary functions handling numeric values
(define (num+ n1 n2)
  (def (numV v1) n1) (def (numV v2) n2) (numV (+ v1 v2)))

(define (num-zero? n)
  (def (numV v) n) (zero? v))


;; interp :: Expr Env Store -> Value*Store
(define (interp expr env sto)
  (match expr

    [(num n) (v*s (numV n) sto)]
    
    [(id x) (v*s (lookup-sto (lookup-env x env) sto) sto)]
    
    [(fun id body) (v*s (closureV id body env) sto)]

    [(if0 c t f)
     (def (v*s c-val c-sto) (interp c env sto))
     (if (num-zero? c-val)
         (interp t env c-sto)
         (interp f env c-sto))]
    
    [(add l r)
     (def (v*s l-val l-sto) (interp l env sto))
     (def (v*s r-val r-sto) (interp r env l-sto))
     (v*s (num+ l-val r-val) r-sto)]
 
    [(app fun-expr arg-expr)
     (def (v*s (closureV id body fenv) fun-sto) (interp fun-expr env sto))
     (def (v*s arg-val arg-sto) (interp arg-expr env fun-sto))
     (def new-loc (next-location arg-sto))
     (interp body
             (extend-env id new-loc fenv)
             (extend-sto new-loc arg-val arg-sto))]

    [(seqn expr1 expr2)
     (def (v*s _ sto1) (interp expr1 env sto))
     (interp expr2 env sto1)]
    [(newbox val-expr)
     (def (v*s val-val val-sto) (interp val-expr env sto))
     (def new-loc (next-location val-sto))
     (v*s (boxV new-loc)
          (extend-sto new-loc val-val val-sto))]
    [(openbox box-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (v*s (lookup-sto loc box-sto) box-sto)]
    [(setbox box-expr val-expr)
     (def (v*s (boxV loc) box-sto) (interp box-expr env sto))
     (def (v*s val-val val-sto) (interp val-expr env box-sto))
     (v*s val-val
          (extend-sto loc val-val val-sto))]))



;; run :: s-expr -> value
(define (run prog)
  (def (v*s v _) (interp (parse prog) empty-env empty-sto))
  v)



;; some testing
(print-only-errors #t)
(define expr1 '(with (b (newbox 0))
                     (seqn (setbox b (+ 1 (openbox b)))
                           (openbox b))))
(test (run expr1) (numV 1))
(define expr2 '(with (a (newbox 1))
                    (with (f (fun (x) (+ x (openbox a))))
                          (seqn (setbox a 2) (f 5)))))
(test (run expr2) (numV 7))
(define expr3 '(with (a (newbox 1))
                     (seqn (with (b 3) b)
                           b)))
(test/exn (run expr3) "free identifier")
(define expr4 '(with (b (newbox 0))
                    (if0 (seqn (setbox b 5) (openbox b))
                         1
                         (openbox b))))
(test (run expr4) (numV 5))
(define expr5 '(with (b (newbox 4))
                     (+ (openbox b)
                        (with (dummy (setbox b 5))
                              (openbox b)))))
(test (run expr5) (numV 9))
(define expr6 '(with (switch (newbox 0))
                     (with (toggle (fun (dum)
                                        (if0 (openbox switch)
                                             (seqn
                                              (setbox switch 1)
                                              1) (seqn
                                                  (setbox switch 0)
                                                  0))))
                           (+ (toggle 1729)
                              (toggle 1729)))))
(test (run expr6) (numV 1))