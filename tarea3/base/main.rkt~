#lang play

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
  ;(this) ; acceder al objeto actual
  ) 

;; values
;;extendemos Val con la clausura de metodos de objetos
(deftype Val
  (numV n)
  (boolV b)
  (mclosV id args body method-env) ; clausura para los metodos 
  (objclosV fields methods)) 

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
    
    [(list 'object  m ...)
     (object (map parse-member m))]

    [(list 'send o msg args ...)
     ;(send (parse o) (parse mid) (map parse args))]
     (send o msg (map parse args))]
    
    [(list 'get f)
     (get  f)]
    
    [(list 'set f e)
     (set f (parse e))]
    ))

;; parse-member :: s-expr -> Member
(define (parse-member s-expr)
  (match s-expr
    [(list 'field id b) (field id (parse b))]
    [(list 'method id (list args ...) b) (method  id args (parse b))]
    ))




;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP INTERP 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interp :: Expr Env -> Val
(define (interp expr env [o object] [aux-env mtEnv])
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
    
    [(id x)
     (if (equal? mtEnv aux-env)
         (env-lookup x env)
         (interp (env-lookup x aux-env) env))



     (interp (env-lookup x aux-env) env)
     #;(begin
         (def found-in-aux (env-lookup x aux-env))
         (if found-in-aux (interp found-in-aux env)  (env-lookup x env)))
     ] ;; aqui esta el problema
    
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
       ) ; 2) aqui se interpreta send con el objeto en el new-env (ocurrio 1)
     ]
    
    [(field f-id f-val) (cons f-id (interp f-val env))]

    ;[(method m-id m-args m-body) (cons m-id (λ(m-args) m-body))]
    ;[(method m-id m-args m-body) (cons m-id (cons m-body m-args))]
    

    [(object members)
     (def o-fields (filter field? members))
     (def o-methods (filter method? members))
     (def field-hash (make-hash))
     (for-each
      (λ(m) (let ([new-field (interp m env)]);interpretando con ambiente global (env)
              (hash-set! field-hash (car new-field) (cdr new-field))) #t)
      o-fields)
     (objclosV field-hash o-methods)
     ]



    [(mclosV id params body m-env)
     (mclosV id params body m-env)
     ]
    
    [(method m-id m-args m-body) (method m-id m-args (interp m-body env o aux-env))]
    
    [(send obj msg args)
     ;env ; aqui se encuentra el objeto al que le queremos enviar msg
     ;obj; entrega 'o
     ;(env-lookup obj env) ; esto nos entrega objclosV
     (def (objclosV field-hash method-list) (env-lookup obj env))
     ; method-list
     (def searched-method (findf (lambda(m) (equal? msg (method-id m))) method-list))
     ;searched-method
     (def clos-method (closure-method searched-method args))
     ;clos-method
     ;(interp clos-method env obj)
     (def (mclosV id params body m-env) (interp clos-method env))
     ;m-env
     ;(interp clos-method env obj)
     ;searched-method
     ;body
     (interp body env obj m-env) ;; descomentar linea 
     ;(def )
     ;(def ready-method (interp searched-method m-env obj)) ; aqui estoy
     ;ready-method
     ;(def (method r-id r-params r-body) ready-method )
     ;r-body
     ;(interp r-body env obj)
     ;(env-lookup o env)
     ;(interp ready-method env)
     ;(if searched-method (process-method searched-method args o) (error "message not found: " msg))  
     #;(if searched-method (begin
                             (let ([closmethod (closure-method searched-method args)])
                               closmethod
                               ;(apply-method closmethod o env)
                               ))
           (error "message not found: " msg))     
     ;(def closmethod (closure-method found-method args))
     ;closmethod
     ;(map (λ(m)()) method-list)
     ;(def closure-methods (list-of-closure-methods method-list args))
     ;closure-methods
     ;(def mclosV-list (closurize-method-list method-list args))
     ;mclosV-list
     ;(def list-pair-id-method (map (λ(m) (interp m env o)) method-list))
     ;(def found-msg (assoc msg list-pair-id-method)) ; TODO: Catch Error if not found
     ;((cdr found-msg) args)
     ]
    [(get f)
     (def (objclosV field-hash method-list) (env-lookup o env))
     (def found-field (hash-ref! field-hash f (λ () (error "field not found: " f))))
     found-field
     ]
       
    [(set f new-val)
     (def interp-val (interp new-val env o aux-env))
     ;interp-val
     ;(env-lookup o aux-env) esto me entrega el objeto que quiero modificar
     (def (objclosV field-hash method-list) (env-lookup o env))
     
     (begin  ; este bloque cambia con exito el valor de 'y en obj
       (hash-set! field-hash f interp-val)
       (env-lookup o env) ;; muestra el objeto modificado (borrar esta linea)
       )
     ]
    [(aEnv h rEnv) (aEnv h rEnv)]
    ))

(define (process-method searched-method args obj)
  (def (mclosV id params m-body method-env) (closure-method searched-method args))
  (def f (match-vals m-body method-env obj))
  method-env
  )

(define (match-vals method env obj)
  (match method
    [(set id val) (set id (interp val env obj))]
    ))

;(set id (interp val env))































(define (apply-method clo-method obj up-env)
  (match clo-method
    [(mclosV id args body env)
     (values id args  body  env obj up-env)
     ]))




(define (closure-method m user-args)
  (match m
    [(method id args body)
     (def method-env (multi-extend-env args user-args mtEnv))
     (mclosV id args body method-env)
     ]))












#;(define (list-of-closure-methods mlist args-list)
    (map (λ()) mlist args-list)
    #;(let ([method-env (multi-extend-env '() '() mtEnv)]))
    )
(define (closurize-method method arg)
  method)
#;(if (= (length mlist) (length args-list))
      (aEnv (make-immutable-hash (map cons mlist args-list)) method-env)
      (error "wrong_input, mismatched lengths"))

#;(if (= (length ids) (length exprs))
      (aEnv (make-immutable-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths"))




















;; interp-fields :: list -> hash
#;(define (interp-fields flist)
    (match flist
      []))





;; TODO
(define (interp-field fld)
  (match fld
    [(numV n) n]
    [(boolV b) b]))




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


;; look-inside-env
(define (look-inside-env)
  (def parsed_local (parse '(local
                              [(define o (object
                                          (field x 1)
                                          (field y 2)
                                          (method sum (z) (+ (get x) (+ (get y) z)))
                                          (method double-y () (+ (get y) (get y)))
                                          (method set-x (val) (set x val))
                                          (method set-y (val) (set y val))
                                          (method get-y () (get y))
                                          ))]
                              ;(send o sum 3)
                              ;(send o double-y)
                              ;(send o get-y)
                              ;(send o set-y 1000)
                              (seqn
                               (send o set-x (+ 1 3))
                               (+ (send o sum 3) (send o get-y)))
                              )))



  
  #;(aEnv (hash 'val (num 1000)) mtEnv)




  
  ;parsed_local
  (def interp_local (interp parsed_local mtEnv))
  (values parsed_local interp_local)
  ;(def o-list-of-methods (interp parsed_local mtEnv)); send nos entrega una lista de
  
  ; metodos hasta el momento
  ;(def list-pair-id-method (map (λ(m) (interp m mtEnv)) o-list-of-methods))
  ;(values o-list-of-methods)
  ;(def five (assoc 'five list-pair-id-method))
  ;(values o-list-of-methods list-pair-id-method five (apply (cdr five) (list 0)))
  )

;(apply (cdr pair-id-method) (list 0))






#;(define (send-msg o msg)
  
    )

(define (get-object p)
  (match p
    ([lcal l body] (get-object-from-list (first l)))))

(define (get-object-from-list l)
  (match l
    ([my-def id e] e)))

(define (get-env-from obj env)
  (match obj
    ([object members] ; tengo que entregar un valor
     (let ([obj-env (multi-extend-env '() '() mtEnv)])
       (for-each
        (λ(m) (let ([new-member (interp m env)]);interpretando con ambiente global (env)
                (extend-frame-env! (car new-member) (cdr new-member) obj-env)) #t)
        members)
       obj-env))))













