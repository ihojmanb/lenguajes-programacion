#lang play
(print-only-errors #t)

;; sintaxis abstracta: la representación que ocupa el interprete para
;; darle semántica al código. 
;; <expr> ::= (num <num>)
;;         |  (sum <expr> <expr>)
;;         |  (sub <expr> <expr>)
;;         |  (if0 <expr> <expr> <expr>)
;;         |  (with <id> <expr> <expr>)
;;         |  (id <id>)
;;         |  (app <id> <expr>)

;;tipo inductivo para representar expresiones aritmeticas
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (sub l r)
  (if0 c l r)
  (with id expr body)
  (app f-name f-arg))

;; sintaxis concreta: como el programador escribe el programa
;; <s-expr> ::= <num>
;;           | (list '+ <s-expr> <s-expr>)
;;           | (list '- <s-expr> <s-expr>)
;;           | (list 'if0 <s-expr> <s-expr> <s-expr>)
;;           | (list 'with (list <sym> <s-expr>) <s-expr>)
;;           | <sym>
;;           | (list <sym> <s-expr>)

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
    [(list if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with (list id expr) body) #:when (symbol? id)
                                      (with id (parse expr) (parse body))]
    [(list f a) #:when (symbol? f) (app f (parse a))]
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
;; empty-env :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value
(deftype Env
  (mtEnv)
  (aEnv id val next))
(define empty-env mtEnv)
(define (extend-env id val env)
  (aEnv id val env))
(define (lookup-env id env)
  (match env
    [(mtEnv) (error("free identifier: " id))]
    [(aEnv x v n) (if (equal? id x)
                      v
                      (lookup-env id n))]))
;; Intérprete
;; eval :: Expr listof(FunDef) Env -> number
;; evaluates an arithmetical expression with function calls
;; and local definitions deferring the substitutions
(define (eval expr f-list env)
  (match expr
    [(num n) n]
    [(id x) (lookup-env x env)]
    [(add l r) (+ (eval l f-list) (eval r f-list))]
    [(sub l r) (- (eval l f-list) (eval r f-list))]
    [(if0 c t f) (if (zero? (eval c f-list))(eval t f-list) (eval f f-list))]
    [(with x e b) (def new-env (extend-env x (eval e f-list env) env))
                  (eval b f-list new-env)]
    [(app f e)
     (def (fundef _ arg body) (look-up f f-list))
     (def new-env (extend-env arg (eval e f-list env) mtEnv));; using mtEnv instead of env
     ;; we are enabling static scope instead of dynamic: we only have access to the
     ;; variables in or lexicall block, not in the upper calling stack
     (eval body f-list new-env)
     e]
    [(id x) (error 'eval "free identifier ~a" x)]
    ))

(define (run prog f-list)
  (eval (parse prog) f-list))




