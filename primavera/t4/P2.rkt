;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  BASE - P2                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Nombre y Apellido: Ianiv Hojman
- Modificar y guardar como P2.rkt
- Recuerde hacer los tests en P2_tests.rkt
|#

#lang play
(print-only-errors #t)
#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym list> <expr>)
         | (app <expr> <expr list>)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun args body)
  (app f-name f-args)
  (my-set id val))



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
           | (list 'seq (list <s-expr> <s-expr>)) <- syntactical sugar
           | (list 'set <sym> <s-expr>)
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
    [(list 'fun args b) (fun args (parse b))
                        #;(if (> (length args) 1)
                              (fun (map parse args) (parse b))
                              (fun (first args) (parse b)))]
    [(list f a) 
     (match a
       [(? number?) (app (parse f) (parse a))]
       [(? list?) (app (parse f) (map parse a))])
     
     ]    
    [(list 'with (list x e) b) #:when (symbol? x)
                               (app (parse (list 'fun (list x) b)) (parse e))]
    [(list 'seq l r) (parse (list 'with (list '_ l) r))]
    [(list 'set x val) (my-set (parse x) (parse val))])) 



;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env :: Env
;; extend-env :: Id Loc Env -> Env
;; lookup-sto :: Id Env -> Loc

;; Implementation of the ADT

;; <env> ::= mtEnv
;;        | (aEnv <id> <loc> <env>)
(deftype Env
  (mtEnv)
  (aEnv id loc env))

(define empty-env (mtEnv))
(define extend-env aEnv)

(define (lookup-env x env)
  ;; printear x para ver si es 'a o (id 'a)
  (display "lookup-env: ")
  (displayln x)
  (displayln env)
  (match env
    [(mtEnv) (error 'lookup-env "free identifier ~a" x)] 
    [(aEnv id loc rest)
     (if (symbol=? id x)
         loc
         (lookup-env x rest))]))


;; empty-sto :: Store
;; extend-sto :: Loc Value Store -> Store
;; lookup-sto :: Loc Store -> Value

;; Implementation og the ADT

;; <sto> ::= mtSto
;;        | (aSto <loc> <value> <sto>)
(deftype Store
  (mtSto)
  (aSto loc value sto))

(define empty-sto (mtSto))
(define extend-sto aSto)

(define (lookup-sto x sto)
  (match sto
    [(mtSto) (error 'lookup-sto "No value at location -a" x)]
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
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric values
;; binop :: (Num Num -> Num) -> (Value Value -> Value)
;; Lifts a binary numeric operator to (numeric) Value's 
(define (binop op)
  (λ (n1 n2)
    (def (numV v1) n1) (def (numV v2) n2) (numV (op v1 v2))))

;; unop :: (Num -> A) -> (Value -> A)
;; Lifts a function over Num to (numeric) Value 
(define (unop op)
  (λ (n) (def (numV v) n) (op v)))

(define (num-zero? n)
  (def (numV v) n) (zero? v))


;; Pair of values and stores
;; <value*store> ::= (v*s <val> <sto>)
(deftype Value*Store
  (v*s val sto))


;; eval :: Expr Env Store -> Value*Store
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]
    [(fun id body) (v*s (closureV id body env) sto)]
    [(id x) (v*s (lookup-sto (lookup-env x env) sto) sto)]
    [(add l r)
     (display "env when adding: ")
     (displayln env)
     (def (v*s l-val l-sto) (eval l env sto))
     (def (v*s r-val r-sto) (eval r env l-sto)) ;; aquí se evalua (id 'a)
     (v*s ((binop +) l-val r-val) r-sto)]
    [(sub l r)
     (def (v*s l-val l-sto) (eval l env sto))
     (def (v*s r-val r-sto) (eval r env l-sto))
     (v*s ((binop -) l-val r-val) r-sto)]
    [(if0 c t f)
     (def (v*s c-val c-sto) (eval c env sto))
     (if (num-zero? c-val)
         (eval t env c-sto)
         (eval f env c-sto))]

    [(app fun-expr arg-expr)
     (match arg-expr
       #;[(? list?)
        (def (v*s (closureV id body fenv1) fun-sto1) (eval fun-expr env sto))
        ;; y = 'a
        (displayln "fenv of 1st arg:")
        (displayln fenv1)
        (def (v*s arg-val1 arg-sto1) (eval (first arg-expr) env fun-sto1))
        (def new-loc1 (next-location arg-sto1)) 
        (def recEnv1 (extend-env (first id) new-loc1 fenv1)) 
        (def recSto1 (extend-sto new-loc1 arg-val1 arg-sto1))
        
        ;; x = 8
        (displayln "env of 2nd arg:")
        (displayln recEnv1)
        (def (v*s arg-val2 arg-sto2) (eval (second arg-expr) env fun-sto1)) ;; fun-sto1? o arg-sto1?
        (def new-loc2 (next-location recSto1))  
        (def recEnv2 (extend-env (second id) new-loc2 recEnv1))
        (def recSto2 (extend-sto new-loc2 arg-val2 recSto1))

        ;; z = 5
        (def (v*s arg-val3 arg-sto3) (eval (third arg-expr) env fun-sto1))  ;; fun-sto1? o arg-sto2?
        (def new-loc3 (next-location recSto2))  
        (def recEnv3 (extend-env (third id) new-loc3 recEnv2))
        (def recSto3 (extend-sto new-loc3 arg-val3 recSto2))
        (eval body recEnv3 recSto3)
        ]
       [(? list?)
          (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto))
          (display "first real argument: ")
          (displayln (first arg-expr))
          (displayln "\n")
          (displayln "env of 1st arg:")
          (displayln env)
          (def (v*s arg-val arg-sto) (eval (first arg-expr) env fun-sto))
          (def new-loc (next-location arg-sto)) 
          (def recEnv (extend-env (first id) new-loc fenv)) ;; este env no viola el scope estático? (antes era fenv) 
          (def recSto (extend-sto new-loc arg-val arg-sto))  

          (def (v*s (closureV _ _ finalEnv) finalSto)
            (build-env-sto (rest id) (rest arg-expr) fun-sto env recEnv recSto))
          (eval body finalEnv finalSto)
          ]
       #;[(? list?) ;; safe para no cagarla xd
          (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto))
          (def (v*s arg-val arg-sto) (eval (first arg-expr) env fun-sto))
          (def new-loc (next-location arg-sto))
          (eval body
                (extend-env (first id) new-loc fenv)
                (extend-sto new-loc arg-val arg-sto))
          ]
       [_ ;; cualquier otro caso
        (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto))
        (def (v*s arg-val arg-sto) (eval arg-expr env fun-sto))
        (def new-loc (next-location arg-sto))
        (display "extended env: ")
        (displayln (extend-env (first id) new-loc fenv))
        (eval body
              (extend-env (first id) new-loc fenv)
              (extend-sto new-loc arg-val arg-sto))])]
    
    [(my-set x newval)
     (def (v*s val-val val-sto) (eval newval env sto))
     (def loc (lookup-env (id-x x) env))
     (v*s val-val
          (extend-sto loc val-val val-sto))
     ]))
;; build-env-sto :: TODO
;; TODO TESTS
(define (build-env-sto idlist arglist fun-sto env recEnv recSto)
  (match arglist
    ['() (v*s (closureV 'dummy1 'dummy2 recEnv) recSto)]
    [_
     (def (v*s arg-val arg-sto) (eval (first arglist) env fun-sto)) ;; que pasa cuando hago (eval (id 'a) env sto???)
     (def new-loc (next-location recSto))
     (def newRecEnv (extend-env (first idlist) new-loc recEnv)) ;; env que le paso a la funcion recursiva
     (def newRecSto (extend-sto new-loc arg-val recSto)) ;; sto que le paso a la funcion recursiva
     (build-env-sto (rest idlist) (rest arglist) fun-sto env newRecEnv newRecSto)
     ]))



;; is-ref-expr?:: sym -> boolean
;; Retorna verdadero si un simbolo corresponde a una expresion de referencia ('&var), o falso sino.
(define (is-ref-expr? expr)
  (def symstr (symbol->string expr))
  (equal? (string-ref symstr 0) #\&)) 

;; get-id:: sym -> sym
;; Retorna el valor de una id de referencia ('&var) en su id real ('var).
(define (get-id id)
  (string->symbol (substring (symbol->string id) 1)))


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
;; run :: s-expr -> value
(define (run prog)
  ;(print (parse prog))
  (def (v*s v _) (eval (parse prog) empty-env empty-sto))
  v)


;; some testing
(define expr1 '(with (f (fun (y) y)) (f (4))))
(test (run expr1) (numV 4))
(define expr2 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (f (4)))))
(test (run expr2) (numV 7))
(define expr3 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (with (x 5) (+ x (f (4)))))))
(test (run expr3) (numV 12))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;a)
(test (run '(seq (+ 1 2) 5)) (numV 5))
;b)
(test (run '(with (x 9)
                    ( if0 (seq (set x 2) (- 3 3))
                          (+ 1 x)
                          (- 14 x))))
        (numV 3))

;c)
(test (run'(with (f (fun (z) (+ 1 z)))
                   (f (8)))) (numV 9))

(define expr4 '(with (f (fun (x y z) (+ (- x y) z)))
                       (f (8 3 7))))
(test (run expr4) (numV 12))

;; funcion sin argumentos
#;(test (run '(with (f (fun () (+ 3 2)))
                    (f ()))) (numV 5))

(define expr5 '(with (f (fun (y x z) (seq (set y 10) (+ x z))))
                     (with (a 3)
                           (+ (f (a 8 5)) a))))
(test (run expr5)
      (numV 16))


(define expr6 '(with (f (fun (x y z) (seq (set y 10) (+ x z))))
                     (with (a 3)
                           (+ (f (8 a 5)) a))))

(test (run expr6)
      (numV 16))

(define expr9 '(with (a 3)
                       (with (f (fun (x y z) (seq (set y 10) (+ x z))))
                             (+ (f (8 a 5)) a))))
(test (run expr9)
        (numV 16))






