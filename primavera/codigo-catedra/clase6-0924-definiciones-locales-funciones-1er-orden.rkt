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

;; subst :: Expr symbol Expr -> Expr
;; sustituye en 'in' cada ocurrencia libre
;; de 'what' por 'for'
(define (subst in what for)
  (match in
    [(num n) (num n)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(with x e b) (with x
                        (subst e what for)
                        (if (symbol=? x what)
                            b
                            (subst b what for)))]
    [(id x) (if (symbol=? x what)
                for
                (id x))]
    [(app f e) (app f (subst e what for))]))

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


;; Intérprete
;; eval :: Expr listof(FunDef) -> number
;; evaluar una expresion aritmetica
(define (eval expr f-list)
  (match expr
    [(num n) n]
    [(id x) x]
    [(add l r) (+ (eval l f-list) (eval r f-list))]
    [(sub l r) (- (eval l f-list) (eval r f-list))]
    [(if0 c t f) (if (zero? (eval c f-list))(eval t f-list) (eval f f-list))]
    [(with id expr body) (eval (subst body id (num (eval expr f-list))) f-list) ]
    [(app f e)
     (def (fundef _ arg body) (look-up f f-list))
     (eval (subst body arg (num (eval e f-list))) f-list)]
    [(id x) (error 'eval "free identifier ~a" x)]
    ))

(define (run prog f-list)
  (eval (parse prog) f-list))



(define my-funcs
  (list (fundef 'my-double 'x (parse '(+ x x)))
        (fundef 'my-plus1 'x (parse '(+ x 1)))
  (fundef 'my-S 'n (parse (list if0 'n 0 '(+ n (my-S (- n 1)))))))) 

(define my-expr '(+ 1 (my-double (my-plus1 4))))
(define my-example '(my-S 5))
(test (run my-expr my-funcs) 11)










