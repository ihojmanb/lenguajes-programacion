;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  BASE - P2                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Nombre y Apellido: Ianiv Hojman
|#

#lang play
(print-only-errors #t)
#|
<expr> ::= (num <num>)
         | (ref <id>)                 ;; referencia a un identificador
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
  (ref x)
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
           | (list '& <sym>)
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
    [ n #:when (number? n) (num n)]
    [ x #:when (symbol? x) (if (is-ref-expr? x)
                               (ref x)
                               (id x))]
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
  (closureV id body env)
  (refclosureV id body env))

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
    #;[(ref x) ()]
    [(add l r)
     (print "env while adding: ")
     (print env)
     (displayln "\n")
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
       ['() ;; aplicación de una función sin argumentos
        (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto))
        (eval body
              fenv
              fun-sto)]
       [(? list?) ;; qué pasa cuando el primer argumento es un (ref x)?
        (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto))
        (match (first arg-expr)
          [(ref x)
           (println "REF")
           (println "env: ")
           (println env)
           (println "fenv: ")
           (println fenv)
           (def xloc (lookup-env (get-id x) env))
           (def dummy-loc (next-location sto)) ;; agregamos una variable dummy al store
           ;; para que el tamaño del storage crezca a la par con el ambiente y las locaciones no se solapen.
           ;; como no habrá un id enlazado a esta locación, no tendremos acceso a ella, y por lo tanto no afecta
           ;; al comportamiento del Env y Sto
           (def (v*s (closureV _ _ finalEnv) finalSto)
             (build-env-sto (rest id) (rest arg-expr) fun-sto env
                            (extend-env (first id) xloc fenv)
                            (extend-sto dummy-loc '_ sto)));; antes tenía fun-sto ;; OJO, es este el sto
           ;; que tengo que extender?
           (eval body finalEnv finalSto)
           
           ]
          [_
           (def (v*s arg-val arg-sto) (eval (first arg-expr) env fun-sto))
           (def new-loc (next-location arg-sto)) 
           (def recEnv (extend-env (first id) new-loc fenv)) ;; 
           (def recSto (extend-sto new-loc arg-val arg-sto))  
           (println "env while evaluating list: ")
           (println env)
           (def (v*s (closureV _ _ finalEnv) finalSto)
             (build-env-sto (rest id) (rest arg-expr) fun-sto env recEnv recSto))
           (eval body finalEnv finalSto)
           ]
          )
        ]
       #;[(? list?) ;; SAFE NO TOCAR
          (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto)) 
          (def (v*s arg-val arg-sto) (eval (first arg-expr) env fun-sto))
          (def new-loc (next-location arg-sto)) 
          (def recEnv (extend-env (first id) new-loc fenv)) ;; 
          (def recSto (extend-sto new-loc arg-val arg-sto))  
          (println "env while evaluating list: ")
          (println env)
          (def (v*s (closureV _ _ finalEnv) finalSto)
            (build-env-sto (rest id) (rest arg-expr) fun-sto env recEnv recSto))
          (eval body finalEnv finalSto)
          ]
       [_ ;; cualquier otro caso
        (println "eval app: arg-expr is NOT a list ")
        (def (v*s (closureV id body fenv) fun-sto) (eval fun-expr env sto))
        (def (v*s arg-val arg-sto) (eval arg-expr env fun-sto))
        (print "arg-sto: ")
        (println arg-sto)
        (print "fenv: ")
        (println fenv)
        (def new-loc (next-location arg-sto))
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
     (match (first arglist)
       [(ref x)
        (def xloc (lookup-env (get-id x) env))
        (def dummy-loc (next-location recSto)) ;; agregamos una variable dummy al storage
        ;; para que el tamaño del storage crezca a la par con el ambiente y las locaciones no se solapen.
        ;; como no habrá un id enlazado a esta locación, no tendremos acceso a ella, y por lo tanto no afecta
        ;; al comportamiento del Env y Sto
        (build-env-sto (rest idlist) (rest arglist) fun-sto env
                       (extend-env (first idlist) xloc recEnv)
                       (extend-sto dummy-loc '_ recSto)) ;; antes tenía fun-sto
        ]
       [_
        (def (v*s arg-val arg-sto) (eval (first arglist) env fun-sto)) ;; que pasa cuando hago (eval (id 'a) env sto???)
        (def new-loc (next-location recSto))
        (def newRecEnv (extend-env (first idlist) new-loc recEnv)) ;; env que le paso a la funcion recursiva
        (def newRecSto (extend-sto new-loc arg-val recSto)) ;; sto que le paso a la funcion recursiva
        (build-env-sto (rest idlist) (rest arglist) fun-sto env newRecEnv newRecSto)
        ]
       )
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
