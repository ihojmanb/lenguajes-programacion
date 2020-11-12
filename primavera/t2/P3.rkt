;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P3 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO: IANIV HOJMAN
;; Mucho éxito :)

#lang play
(print-only-errors #t)

#| ================================
                PARTE A
   ================================|#
#|
<expr> ::=( real <num>)
         |(comp <num> <num>)
         |(add <expr> <expr>)
         |(id <id>)
         |(fun <sym> <expr>)
         |(app <expr> <expr>)
|#
;; Expr representa la sintaxis abstracta del lenguaje
(deftype Expr
  (real r)
  (comp r i)
  (add e1 e2)
  (id x)
  (fun arg body)
  (app f a))
#| ================================
                PARTE B
   ================================ |#

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+ <num> (list <num>) 'i)
           | (list '+ <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list <s-expr> 'where <sym> '= <s-expr>)  <- syntactical sugar
|#
;; parse :: s-expr -> Expr
;; traduce la sintaxis concreta a sintaxis abstracta
(define (parse s)
  (match s
    [(? number?) (real s)]
    [(? symbol?) (id s)]
    [(list '+ n1 (list n2) 'i) (comp n1 n2)]
    [(list '+ e1 e2) (add (parse e1) (parse e2))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list b 'where i '= e)
     (app (fun i (parse b)) (parse e))]))


#| ================================
                PARTE C
   ================================ |#

;; Values of Expressions
;; <value> ::= (realV <num>)
;;           | (compV <num> <num>)
;;           | (closureV <sym> <expr> <env>)
;;Value representa los posibles valores que devuelve el lenguaje
(deftype Value
  (realV r)
  (compV r i)
  (closureV arg body env))

;; num+ :: Value Value -> Value
;; num+ toma dos valores del lenguaje, suma las componentes correspondientes
;; y devuelve un Value
(define (sum+ v1 v2)
  (cond
    [(and (realV? v1) (realV? v2))
     (def r1 (realV-r v1))
     (def r2 (realV-r v2))
     (realV (+ r1 r2))]
    [(and (realV? v1) (compV? v2))
     (def r1 (realV-r v1))
     (def r2 (compV-r v2))
     (def i (compV-i v2))
     (compV (+ r1 r2) i)]
    [(and (compV? v1) (realV? v2))
     (def r1 (compV-r v1))
     (def i (compV-i v1))
     (def r2 (realV-r v2))
     (compV (+ r1 r2) i)]
    [(and (compV? v1) (compV? v2))
     (def r1 (compV-r v1))
     (def i1 (compV-i v1))
     (def r2 (compV-r v2))
     (def i2 (compV-i v2))
     (compV (+ r1 r2) (+ i1 i2))]
    [else (error "estas sumando tipos incompatibles")]
    )
  ) 

#| ================================
                PARTE D
   ================================ |#

;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
#|
<env> ::= (mtEnv)
| (aEnv <id> <Value> <env>)
|#
;; Env define los ambienten que nos ayudarán con la substitución diferida
;; podemos tener un ambiente vacio, o uno que contenga simbolos asociados a LValues

(deftype Env
  (mtEnv)
  (aEnv id val next))

;; empty-env  :: Env
;; empty-env entrega un ambiente vacio
(define empty-env (mtEnv))


;; extend-env :: Symbol Value Env -> Env
;; extend-env toma un ambiente y extiende ese ambiente con un simbolo y
;; un Value asociados. Devuelve el nuevo ambiente extendido
(define (extend-env id val env) (aEnv id val env))


;; env-lookup :: Symbol Env -> Value
;; env-lookup toma un símbolo y lo busca en ambiente que se le entrega
;; si lo encuentra, retorna el valor asociado a ese simbolo. Si no, entrega
;; un error
(define (env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "error: ~a no está definido" id)]
    [(aEnv i v n) (if (equal? id i)
                      v
                      (env-lookup id n))]))

(def env1 (aEnv 'x (realV 2) (mtEnv)))
(def env2 (aEnv 'x (realV 2) (aEnv 'y (compV 4 8) (aEnv 'z (closureV 'x (id 'x) (mtEnv)) (mtEnv)))))

;; eval :: Expr Env -> Value
;; computa los valores del lenguaje. Devuelve un Value
(define (eval e env)
  (match e
    [(real r) (realV r)]
    [(comp r i) (compV r i)]
    [(add e1 e2) (sum+ (eval e1 env) (eval e2 env))]
    ;[(id x) (eval (env-lookup x env) env)]
    [(id x) (env-lookup x env)]
    [(fun i b) (closureV i b env)]
    [(app f arg)
     (def (closureV farg fbody fenv) (eval f env))
     (def newEnv (extend-env farg (eval arg env) fenv))
     (eval fbody newEnv)
     ]
    ))

;; run :: s-expr -> Value
;; genera el pipeline de ejecucion. recibe sintixs concreta y devuelve Value
(define (run s) (eval (parse s) empty-env))



