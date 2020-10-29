#lang play

;; sintaxis abstracta
;; <expr> ::= (num <num>)
;;         |  (sum <expr> <expr>)
;;         |  (sub <expr> <expr>)
;;         |  (if0 <expr> <expr> <expr>)
;;         |  (with <id> <expr> <expr>)
;;         |  (id <id>)

;;tipo inductivo para representar expresiones aritmeticas
(deftype Expr
  (num n)
  (id x)
  (add l r)
  (sub l r)
  (if0 c l r)
  (with id expr body))

;; sintaxis concreta
;; <s-expr> ::= <num>
;;           | (list '+ <s-expr> <s-expr>)
;;           | (list '- <s-expr> <s-expr>)
;;           | (list 'if0 <s-expr> <s-expr> <s-expr>)
;;           | (list 'with (list <sym> <s-expr>) <s-expr>)
;;           | <sym>

(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list id expr) body) #:when (symbol? id)
                                      (with id (parse expr) (parse body))]
    [_ (error "error de parseo")])
  )
;; calc :: Expr -> numver
;; evaluar una expresion aritmetica
(define (calc expr)
  (match expr
    [(num n) n]
    [(id x) x]
    [(add l r) (+ (calc l) (calc r))]
    [(sub l r) (- (calc l) (calc r))]
    [(if0 c l r) (if (zero? (calc c))
                     (calc l)
                     (calc r))]
    #;[(with id expr body) ]
    ))

(define (run prog)
  (calc (parse prog)))