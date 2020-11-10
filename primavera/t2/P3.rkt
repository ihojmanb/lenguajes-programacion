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
;; DUDA: en las fun, se parsea x o se deja intacto?
(define (parse s)
  (match s
    [(? number?) (real s)]
    [(? symbol?) (id s)]
    [(list '+ n1 (list n2) 'i) (comp n1 n2)]
    [(list '+ e1 e2) (add (parse e1) (parse e2))]
    [(list 'fun (list x) b) (fun (parse x) (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list b 'where i '= e)
     (app (fun (parse i) (parse b)) (parse e))]))

(test (parse 8) (real 8))
(test (parse '(+ 1 (2)i)) (comp 1 2))
(test (parse '(+ 3 6)) (add (real 3) (real 6)))
(test (parse 'x) (id 'x))
(test (parse '(fun (x) (+ 2 x))) (fun (id 'x) (add (real 2) (id 'x))))
(test (parse '(f (+ 2 3))) (app (id 'f) (add (real 2) (real 3))))
(test (parse '((+ y 5) where y = (+ 2 4))) (app (fun (id 'y) (add (id 'y) (real 5))) (add (real 2) (real 4))) )
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
    )
  ) 
(test (sum+ (realV 2) (realV 3)) (realV 5))
(test (sum+ (realV 2) (compV 5 1)) (compV 7 1))
(test (sum+ (realV 3) (compV -5 -5)) (compV -2 -5))
(test (sum+ (compV 5 1) (realV -4)) (compV 1 1))
(test (sum+ (compV 3 4) (compV 5 1)) (compV 8 5))
(test (sum+ (compV 0 0) (compV 0 0)) (compV 0 0))



#| ================================
                PARTE D
   ================================ |#

;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value



;; eval :: Expr Env -> Value


;; run :: s-expr -> Value




