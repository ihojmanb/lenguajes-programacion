#lang play
(print-only-errors #t)
(struct exn:no-field exn:fail ()) 
(define (raise-no-field)
  (raise (exn:no-field
          (format "no such field")
          (current-continuation-marks))))
#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)
         | (fun (<id>*) <expr>)
         | (<expr> <expr>*)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA OBJETOS
<expr>  ::= ... (todo lo anterior)
         | (object [: <expr>] <member> ...)
         | this
         | (set <id> <expr>)
         | (get <id>)
         | (send <expr> <id> <expr> ...)
         | (shallow-copy <expr>)
         | (deep-copy <expr>)

<member> ::=
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)
  (binop f l r)
  (unop f s)
  (my-if c tb fb)
  (seqn expr1 expr2)
  (lcal defs body)
  (object membrs) ; objeto tine miembros (fields y methods)
  (send obj msg args) ; send toma un objeto un metodo y 0 o mas argumentos para el metodo
  (get f) ; acceder a un field
  (set f expr) ; modificar un field
  (this) ; acceder al objeto actual
  ) 

;; values
;;extendemos Val con la clausura de metodos de objetos
(deftype Val
  (numV n)
  (boolV b)
  (mclosV id args body method-env) ; clausura para los metodos 
  (objclosV objeto fields methods)) 

(deftype Def
  (my-def id expr))


(deftype Member
  (field id expr)
  (method id args body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type

empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env))

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest) (hash-ref hash x (λ () (env-lookup x rest)))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (hash id val) env)]
    [(aEnv h rEnv) (def hupd (hash-set h id val))
                   (set-aEnv-hash! env hupd)]))


;;  PARSER PARSER PARSER PARSER PARSER PARSER PARSER PARSER PARSER PARSER PARSER 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    ['this (this)]
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]

    [(list 'fun args body)
     (object (list (method 'fun args (parse body))))]
    [(list fname val) (send fname 'fun (map parse (list val) ))]
    
    [(list 'object  m ...)
     (object (map parse-member m))]

    [(list 'send o msg args ...)
     (send o msg (map parse args))]
    
    [(list 'get f)
     (get  f)]
    
    [(list 'set f e)
     (set f (parse e))]

    
    ))

;; parse-member :: s-expr -> Member
;; pattern-matching para el el parseo de los miembros de un objeto. toma un s-expr y devuelve
;; un member
(define (parse-member s-expr)
  (match s-expr
    [(list 'field id b) (field id (parse b))]
    [(list 'method id (list args ...) b) (method  id args (parse b))]
    ))
(test (parse-member (list 'field 'x 5)) (field 'x (num 5)))

;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interp :: Expr Env -> Val
(define (interp expr env [o void] [aux-env mtEnv])
  (match expr
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(binop f l r) (make-val (f (open-val (interp l env o aux-env))
                                (open-val (interp r env o aux-env))))]
    [(unop f s) (make-val (f (open-val (interp s env o aux-env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]

    [(this)
     (def (objclosV objeto field-hash method-list) (env-lookup o env))
     objeto]

    [(id x)
     (if (equal? mtEnv aux-env)
         (env-lookup x env)
         (interp (env-lookup x aux-env) env))]
    
    [(seqn expr1 expr2) (begin
                          (interp expr1 env o aux-env)
                          (interp expr2 env o aux-env)
                          )]
    [(lcal defs body)
     (let ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (def (cons id val) (interp-def x new-env))
                   (extend-frame-env! id val  new-env) ; 1) aqui se agrega el obj al ambiente
                   #t) defs)
       ;new-env
       (interp body new-env)
       ) 
     ]
    
    [(field f-id f-val) (cons f-id (interp f-val env))]
    
    [(set f new-val)
     (def interp-val (interp new-val env o aux-env))
     (def (objclosV objeto field-hash method-list) (env-lookup o env))
     (begin  
       (hash-set! field-hash f interp-val)
       )
     ]
    
    [(get f)
     (def (objclosV objeto field-hash method-list) (env-lookup o env))
     (def found-field (hash-ref! field-hash f (λ () (error "field not found"))))
     found-field
     ]
    [(object members)
     (def o-fields (filter field? members))
     (def o-methods (filter method? members))
     (def field-hash (make-hash))
     (for-each
      (λ(m) (let ([new-field (interp m env)]);interpretando con ambiente global (env)
              (hash-set! field-hash (car new-field) (cdr new-field))) #t) 
      o-fields)
     (objclosV (object members) field-hash o-methods)
     ]

    [(mclosV id params body m-env)
     (mclosV id params body m-env)
     ]
    
    [(method m-id m-args m-body) (method m-id m-args (interp m-body env o aux-env))]
    
    [(send obj-name msg args); send siempre buscar en el env global
     (def (objclosV objeto field-hash method-list) (env-lookup obj-name env)) ;; busco el objeto en el ambiente
     (def searched-method (findf (lambda(m) (equal? msg (method-id m))) method-list)) ;; busco en msg en sus metodos 
     (if searched-method (begin ;; si el método está
                           (let ([clos-method  (closure-method searched-method args env)])
                             clos-method
                             ;(def (mclosV id params body m-env) (interp clos-method env))
                             ;(interp body env obj-name m-env)
                             ))
         (error "method not found"))

     ]
    ))

;; closure-method:: method x list x env -> mclosV
;; closure-method lo que hace es crear un ambiente method-env donde existan las variables del
;; metodo asociadas a los argumentos que le pasa el usuario, y retorna un mclosV,
;; que encapsula metodo + method-env
(define (closure-method m user-args env)
  (match m
    [(method id args body)
     (def method-env (multi-extend-env args user-args env))
     (mclosV id args body method-env)
     ]))

(test (closure-method (method 'set-y '(val) (set 'y (id 'val))) (list 1) empty-env)
      (mclosV 'set-y '(val) (set 'y (id 'val))  (aEnv '#hash((val . 1)) (mtEnv))))



;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr )
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))


















