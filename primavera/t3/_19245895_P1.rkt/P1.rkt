#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <id> <expr>)
         | (app <expr> <expr>)
         | (div <expr> <expr>)
         | (vacio) ;; representa las listas vacias
         | (mtch <id> <list>) ;; representa el match de las listas, donde <id> representa la lista a matchear y <list> la lista de casos
         | (par <expr> <expr>) ;; sintaxis abstracta del constructor cons
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg)
  (div n d)
  (vacio)
  (mtch expr cases)
  (par e1 e2)
  )



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactic sugar
           | (list '/  <s-expr> <s-expr>)
           | (list 'cons <s-expr> <s-expr>) ;; cons toma dos s-expr's
           | (list 'match <sym> 'as <nil> '=> <s-expr> <cons> <sym> <sym> '=> <s-expr>) 
           | 'nil ;; se parsea como lista vacía
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ null #:when (equal? null 'nil) vacio]
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]    
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '/ n d) (div (parse n) (parse d))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
                               (app (fun x (parse b)) (parse e))]
    [(list 'cons h t) (par (parse h) (parse t))]
    [(list 'match x 'as case1 case2) (mtch (parse x)
                                           (if (check-patterns (list case1 case2))
                                               (map parse-case (list case1 case2))
                                               (error 'match "tienes un error con el orden de patrones")
                                               )
                                           )]
    [(list 'match x 'as case1) (error 'match "tienes un error con numero de patrones")]
    ))

;; check-patterns :: List[<s-expr>] -> Bool
;; check-patterns revisa si la lista de casos suplementados es la correcta. si son dos casos, nil y cons x xs,
;; retorna True, de lo contrario retorna False
(define (check-patterns patternList)
  (match patternList
    [(list pat1 pat2)
     (cond
       [(and (eq? 'nil (first pat1)) (eq? 'cons (first pat2))) #t]
       [else #f]
       )
     ]
    [_ #f]
    )
  )

;; parse-case :: s-expr -> Lambda
;                         |List[Expr, <procedure>]
;; parse-case parsea los casos que contiene el match escrito por el programador
;; devolviendo una lambda si la s-expr, que representa el patron que define el programador,
;; es un (nil), o una lista que contiene un cons, encapsulando la cabeza y el resto de la lista
;; y una lambda que recibe dos parametros, en caso de que el patron sea (cons x xs)
(define(parse-case c)
  (match c
    [(list 'nil => body) (λ() (parse body))]
    [(list 'cons x xs => body) (list (cons x xs)(λ (a b) (parse body)))]
    ))

;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup y env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a"y)]
    [(aEnv id val rest) (if (symbol=? id y)
                            val
                            (env-lookup y rest))]))

;; <value> ::= (numV <num>)
;;          |  (fclosureV <id> <expr> <env>)
;;          |  (exprV <expr> <env> <box>)
;;          |  (listV <expr> <expr> <env>)

;; listV funciona muy parecido a fclosureV: envuelve los valores de una lista
;; y el ambiente en el momentó en que se definio aquella lista en un mismo tipo
;; de dato
 
;; values of expressions
(deftype Value
  (numV n)
  (fclosureV id body env)
  (exprV expr env cache)
  (listV head rest env))


;; binop :: (Num Num -> Num) -> (Value Value -> Value)
;; Lifts a binary numeric operator to (numeric) Value's 
(define (binop op)
  (λ (n1 n2)
    (def (numV v1) n1) (def (numV v2) n2) (numV (op v1 v2))))

;; unop :: (Num -> A) -> (Value -> A)
;; Lifts a function over Num to (numeric) Value 
(define (unop op)
  (λ (n) (def (numV v) n) (op v)))

;; Further reduces a Value to a numV or closureV
;; and caches the reduced value
;; strict :: Value -> Value [without exprV]
(define (strict v)
  (match v
    [(exprV expr env cache)
     (if (not (unbox cache))
         (let ([val (strict (eval expr env))])
           (set-box! cache val)
           val)
         (unbox cache))]
    [ _ v]))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given environment
;; using static scope and lazy evaluation
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (fclosureV id body env)]
    [(id x) (env-lookup x env)]
    [(par h t) (listV h t env)]
    [(add l r) ((binop +) (strict (eval l env)) (strict (eval r env)))]
    [(sub l r) ((binop -) (strict (eval l env)) (strict (eval r env)))]
    [(div n d) (if ((unop zero?) (strict (eval d env)))
                   (error 'quotient "undefined for 0")
                   ((binop quotient)
                    (strict (eval n env)) (strict (eval d env))))] ;; ¿optimizar computo duplicado?
    [(if0 c t f) (if  ((unop zero?) (strict (eval c env)))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (fclosureV the-arg the-body the-clos-env) (strict (eval f env)))
               (def the-ext-env (extend-env the-arg
                                            (exprV e env (box #f))
                                            the-clos-env))
               (eval the-body the-ext-env)]
    [(mtch x casos)
     ;(eval-list (eval x env) caselist env)
     ;; ** mirar el comentario de abajo para una explicacion de lo que ocurre**
     (def (exprV e amb b) (eval x env))
     (match e
       [(vacio) ((first casos))]
       [(par h t)
        (def (cons id1 id2) (first (second casos))) ; id1 correspond a x, id2 corresponde a xs
        ;; extendemos el ambiente enlazando id1 e id2 a la cabeza y al resto de la lista, respectivamente.
        ;; notemos que extendemos el ambiente con exprV, es decir, expresiones que no están evaluadas
        ;; con esto respetamos que el lenguaje cumpla con lazy evaluation
        (def newEnv0 (extend-env id1 (exprV h amb (box #f)) (mtEnv))) ; al ocupar amb estamos respetando el scope estatico
        (def newEnv  (extend-env id2 (exprV t (mtEnv) (box #f)) newEnv0)) 
        (eval ((second (second casos)) h t) newEnv) 
        ])
     ]
    ))
;; la lista 'casos' lista de patrones, que contiene un caso de tipo vacio y uno de tipo par .
;; si e matchea el patron vacio, se devuelve el cuerpo del primer caso, que es una lambda
;; si e matchea el patron (par h t), entonces hacemos un inline match del segundo caso, extrayendo
;; los nombres asignados por el programador a la cabeza y la cola de la list, id1 e id2 (que equivalen a x xs, y ys ó
;; como fuera que el programador lo indicó al momento de escribir el programa)
;; una vez con los identificadores de la cabeza y la cola, extendemos el ambiente con las expresiones por evaluar,
;; y evaluamos el cuerpo del segundo caso, que viene dado por (second (second casos)), con los parámetros h y t, pues
;; el cuerpo del segundo caso corresponde a una lambda con dos  argumentos; y consideramos el nuevo ambiente
;; newEnv. La función devuelve un Value


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (eval (parse prog) empty-env))


