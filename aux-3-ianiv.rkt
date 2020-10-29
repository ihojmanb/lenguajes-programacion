#lang play
(print-only-errors #t)
#|
<Expr> ::= <num>
         | {binop op <Expr> <Expr>}
         | {with {<id> <Expr>} <expr>)}
         | <id>

|#
(deftype Expr
  (num val)
  (binop op l r)
  (add l r)
  (sub l r)
  (id x)
  (with-eager id named-expr body)
  (with-lazy id named-expr body))

(define operations (list '+ '- '* '/))
; parse :: sexpr-v -> Expr
(define (parse sexpr)
  (define (operation? op) (member op operations))
  (match sexpr
    [(? number?) (num sexpr)]
    [(? symbol?) (id sexpr)]
    [(list (? operation? op) l r) (binop op (parse l) (parse r))]
    [(list 'with-eager (list i e) b)
     (with-eager i (parse e) (parse b))]
    [(list 'with-lazy (list i e) b)
     (with-lazy i (parse e) (parse b))]
 ))
(test (parse '{with-eager {x 1} {+ x x}})
      (with-eager 'x (num 1) (binop '+ (id 'x) (id 'x))))

;get-op :: binop->op
(define (get-op op)
  (match op
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]))
(test (get-op '+) +)
;Interp :: Expr -> Val
(define (interp expr)
  (match expr
    [(num expr) expr]
    [(id expr) expr]
    [(binop op l r)((get-op op)(interp l) (interp r))]
    [(with-eager id e b)]
    [(with-lazy id e b)]
    ))












; eval :: Expr -> Val
; toma una expresion y la evalua con evaluacion temprana para with-eager
;y con evaluacion perezosa para with-lazy
;(test ())