#lang play
(print-only-errors #t)

;; sintaxis abstracta: la representación que ocupa el interprete para
;; darle semántica al código. 
;; <expr> ::= (num <num>)
;;         |  (add <expr> <expr>)
;;         |  (sub <expr> <expr>)
;;         |  (if0 <expr> <expr> <expr>)
;;         |  (id <id>)
;;         |  (fun <sym> <expr>)
;;         |  (app <expr> <expr>)

;;tipo inductivo para representar la sintaxis abstracta de expresiones aritmeticas
;; con funciones de primera clase
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (sub l r)
  (if0 c l r)
  (fun arg body)
  (app f-name f-arg))

;; sintaxis concreta: como el programador escribe el programa
;; <s-expr> ::= <num>
;;           | <sym>
;;           | (list '+ <s-expr> <s-expr>)
;;           | (list '- <s-expr> <s-expr>)
;;           | (list 'if0 <s-expr> <s-expr> <s-expr>)
;;           | (list <s-expr> <s-expr>)
;;           | (list 'fun (list <sym>) <s-expr> )
;;           | (list 'with (list <sym> <s-expr>) <s-expr>) ;; sintactic sugar


;; el parser es el encargado de transformar la sintaxis concreta
;; en sintaxis abstracta
;; parse :: s-expr -> Expr
;; converts s-expressions to Exprs
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list 'with (list x e) b) #:when (symbol? x)
                               (app (fun x (parse b)) (parse e))]
    [_ (error "error de parseo")])
  )
;; Definition of a function
;; <fundef> ::= (fundef <id> <id> <expr>)
(deftype FunDef
  (fundef name arg body))

;; look-up :: symbol listof(FunDef) -> funDef
;; searches a function definition within a list of definitions
(define (look-up f-name l)
  (match l
    [(list) (error 'lookup "Function ~a not found" f-name)]
    [(cons head tail) (if (symbol=? f-name (fundef-name head))
                          head
                          (look-up f-name tail))]))
(deftype Env
  (mtEnv)
  (aEnv id val next))

;; empty-env :: Env
(define empty-env mtEnv)
;; extend-env :: Symbol Value Env -> Env
(define (extend-env id val env)
  (aEnv id val env))
;; lookup-env :: Symbol Env -> Value
(define (lookup-env id env)
  (match env
    [(mtEnv) (error "free identifier: " id)]
    [(aEnv x v n) (if (equal? id x)
                      v
                      (lookup-env id n))]))


;; values of expressions
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>)
;;          | (exprV <s-expr> <env> <s-expr>)
(deftype Value
  (numV n)
  (closureV id body env)
  (exprV expr env cache))

;; Intérprete
;; eval :: Expr Env -> Value
;; evaluates an arithmetical expression with function calls
;; and local definitions deferring the substitutions
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun arg body) (closureV arg body env)]
    [(id x) (lookup-env x env)]
    [(add l r) (addV (strict (eval l env)) (strict (eval r env)))]
    [(sub l r) (subV (strict (eval l env)) (strict (eval r env)))]
    [(if0 c t f) (if (num-zero? (strict (eval c env)))(eval t env) (eval f env))]
    [(app f e)  (def (closureV farg fbody fenv) (strict (eval f env)))
                (eval fbody (extend-env farg (exprV e env (box #f)) fenv))]
    [(id x) (error 'eval "free identifier ~a" x)]
    ))

;; Further reduces a Value to a numV or closureV
;; strict :: Value -> Value [without exprV]
(define (strict v)
  (match v
    [(exprV expr env cache)
     (if (not (unbox cache))
         (let ([val (strict (eval expr env))])
           (set-box! cache val)
           val)
         (unbox cache))]
    [_ v]))

(define (addV n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(define (subV n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))

(define (num-zero? n)
  (def (numV v) n) (zero? v))

;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (eval (parse prog) empty-env))

; some testing
(define expr1 '(with (f (fun (y) y)) (f 4)))
(test (run expr1) (numV 4))
(define expr2 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (f 4))))
(test (run expr2) (numV 7))
(define expr3 '(with (x 3)
                       (with (f (fun (y) (+ x y)))
                             (with (x 5) (+ x (f 4))))))
(test (run expr3) (numV 12))



