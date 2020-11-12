;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P2 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO:IANIV HOJMAN
;; Mucho éxito :)

#lang play
(print-only-errors #t)
#| <logic> ::= 
    | (bool <bool>)
    | (id <id>)
    | (band <logic> <logic>)
    | (bor <logic> <logic>)
    | (with <id> <logic> <logic>)
|#

#| ================================
                PARTE A
   ================================|#
;; Logic es una representación de operaciones o tipos booleanos
(deftype Logic
  (bool b)
  (id i)
  (band l1 l2)
  (bor l1 l2)
  (with id l1 l2)
  )


#| ================================
                PARTE B
   ================================ |#

#| <s-expr> :: = <bool> 
             | = (list id <sym>)
             | = (list <s-expr> ^ <s-expr>)
             | = (list <s-expr> v <s-expr>)
             | = (list with <sym> <s-expr> <s-expr>)
|#

;; parse :: s-expr -> logic
;; parse toma la sintaxis concreta (definida en la tarea)
;; y devuelve expresiones del tipo Logic
(define (parse sexpr)
  (match sexpr
    [(? symbol?) (cond
                   [(equal? sexpr 'True) (bool #t)]
                   [(equal? sexpr 'False) (bool #f)]
                   [else (id sexpr)])]
    [(list l 'v r) (bor (parse l) (parse r))]
    [(list l '^ r) (band (parse l) (parse r))]
    [(list 'with (list id l1) l2) (with id (parse l1) (parse l2))]
    )
  )

#| ================================
                PARTE C 
   ================================ |#
#| <lvalue> ::= 
    | (BoolV <bool>)
    | (PropV <id>)
    | (ClosureV <sym> <logic> <logic> <Env>)

|#
;; LValue representa todos los tipos que puede entregar los valores del lenguaje,
;; sean booleanos, proposiciones no interpretadas o clausuras de definiciones locales
(deftype LValue
  (BoolV b)
  (PropV p)
  (ClosureV id l1 l2 env) 
  )


#| ================================
                PARTE D
   ================================ |#
#|
<env> ::= (mtEnv)
| (aEnv <id> <LValue> <env>)
|#
;; Env define los ambienten que nos ayudarán con la substitución diferida
;; podemos tener un ambiente vacio, o uno que contenga simbolos asociados a LValues
(deftype Env
  (mtEnv)
  (aEnv id lval next))
;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.

;; empty-env  :: Env
;; empty-env entrega un ambiente vacio
(define empty-env
  (mtEnv))
;; extend-env :: Sym LValue Env -> Env
;; extend-env toma un ambiente y extiende ese ambiente con un simbolo y
;; un LValue asociados. Devuelve el nuevo ambiente extendido
(define (extend-env id val env)
  (aEnv id val env))

;; env-lookup :: Sym Env -> LValue
;; env-lookup toma un símbolo y lo busca en ambiente que se le entrega
;; si lo encuentra, retorna el valor asociado a ese simbolo. Si no, entrega
;; un error
(define (env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "Identificador ~a no definido" id)]
    [(aEnv i v n) (if (equal? id i)
                      v
                      (env-lookup id n))]))
;; interp :: Expr Env -> LValue
;; el interprete toma una expresion y un ambiente, y calcula la expresion retornando
;; un LValue
(define (interp e env)
  (match e
    [(bool e) (cond
                [(equal? e #t) (BoolV e)]
                [(equal? e #f) (BoolV e)])]
    [(id x) (env-lookup x env)]
    [(bor l1 l2)  (my-or (interp l2 env) (interp l1 env))]
    [(band l1 l2) (my-and (interp l2 env)(interp l1 env))]
    [(with id l1 l2)
     (interp l2 (extend-env id (interp l1 env) env))]
    ))

;; my-and :: LValue LValue -> LValue
;; my-and toma dos valores del lenguaje, calcula la operacion 'and' y devuelve un LValue del resultado 
(define (my-and lv1 lv2)
  (def v1 (BoolV-b lv1))
  (def v2 (BoolV-b lv2))
  (BoolV (and v1 v2))
  )
;; my-or :: LValue LValue -> LValue
;; my-or toma dos valores del lenguaje, calcula la operacion 'or' y devuelve un LValue del resultado 
(define (my-or lv1 lv2)
  (def v1 (BoolV-b lv1))
  (def v2 (BoolV-b lv2))
  (BoolV (or v1 v2))
  )











