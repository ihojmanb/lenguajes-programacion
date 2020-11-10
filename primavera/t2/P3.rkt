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
;;DUDA  se parsea i em app?


#| ================================
                PARTE C
   ================================ |#

;; Values of Expressions
;; <value> ::= (realV <num>)
;;           | (compV <num> <num>)
;;           | (closureV <sym> <expr> <env>) ;; DUDA ???
(deftype Value
  (realV r)
  (compV r i)
  (closureV arg body env))
;; esta muy rasca. mejorar este codigo
;; num+ :: Value Value -> Value
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

(deftype Env
  (mtEnv)
  (aEnv id val next))

;; empty-env  :: Env
(define empty-env (mtEnv))
(test empty-env (mtEnv))

;; extend-env :: Symbol Value Env -> Env
(define (extend-env id val env) (aEnv id val env))

(test (extend-env 'x (realV 2) empty-env) (aEnv 'x (realV 2) (mtEnv)))
(test (extend-env 'y (compV 2 -1) empty-env) (aEnv 'y (compV 2 -1) (mtEnv)))

;; env-lookup :: Symbol Env -> Value
(define (env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "error: ~a no está definido" id)]
    [(aEnv i v n) (if (equal? id i)
                      v
                      (env-lookup id n))]))

(def env1 (aEnv 'x (realV 2) (mtEnv)))
;; Test env (faltan)
(test (env-lookup 'x env1) (realV 2))
(test/exn (env-lookup 'y env1) "error: y no está definido")

;; eval :: Expr Env -> Value
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
;; Test eval
(test (eval (real 8) empty-env) (realV 8))
(test (eval (comp 1 2) empty-env) (compV 1 2))
(test (eval (add (real 3) (real 6)) empty-env) (realV 9))
(test/exn (eval (id 'x) empty-env) "error: x no está definido")
(test (eval (id 'y) (aEnv 'y (compV 3 3) empty-env)) (compV 3 3))
(test (eval (fun (id 'x) (add (real 2) (id 'x))) empty-env)
      (closureV (id 'x) (add (real 2) (id 'x)) (mtEnv)))

(test (eval (app (id 'f) (add (real 2) (real 3)))
            (aEnv 'f (closureV 'x (id 'x) (mtEnv)) (mtEnv)))
      (realV 5))
(test/exn (eval (app (id 'f) (add (real 2) (real 3))) empty-env) "error: f no está definido")

(test (eval
         (app (fun 'y (add (id 'y) (real 5))) (add (real 2) (real 4)))
         empty-env)
        (realV 11))
(test (eval
         (app (fun 'y (add (id 'y) (comp 3 1))) (add (real 1) (real 2)))
         empty-env)
        (compV 6 1))


;; Tests parse
(test (parse 8) (real 8))
(test (parse '(+ 1 (2)i)) (comp 1 2))
(test (parse '(+ 3 6)) (add (real 3) (real 6)))
(test (parse 'x) (id 'x))
(test (parse '(fun (x) (+ 2 x))) (fun 'x (add (real 2) (id 'x))))
(test (parse '(f (+ 2 3))) (app (id 'f) (add (real 2) (real 3))))
(test (parse '((+ y 5) where y = (+ 2 4))) (app (fun 'y (add (id 'y) (real 5))) (add (real 2) (real 4))) )


;; testeo de scope estatico; testeo error scope dinamico

;; run :: s-expr -> Value
(define (run s) (eval (parse s) empty-env))
(test (run '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) (compV 6 4))


