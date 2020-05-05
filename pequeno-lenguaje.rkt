#lang play
(print-only-errors #t)

#|
<btree> ::= (lead <val>) |
            (node <val> <btree> <btree>)
|#
(deftype BinTree
  (leaf value)
  (node value left right))

;PATTERN MATCHING
#|
(define (f t)
  (match t
    [(leaf v) ...]
    [(node v l r) ... (f l)... (f r)...]))
|#

;(contains?) :: BinTree x Val -> Bool
(define (contains? t val)
  (match t
    [(leaf v) (equal? v val)]
    [(node v l r) (or (equal? v val)
                      (contains? l val)
                      (contains? r val))]))


;-------------------------------------------

#|
<expr> ::= (num <n>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)

|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r))


#|
<sexpr-v> ::= <n>
          | '(+ <sexpr-v> <sexpr-v>)
          | '(- <sexpr-v> <sexpr-v>)
|#

; parse :: s-expr -> Expr
(define (parse sexpr)
  (match sexpr
    [(? number?) (num sexpr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
 ))

(test (parse 3) (num 3))
(test (parse '{+ 4 2})
      (add (num 4) (num 2)))

(test (parse '{+ 4 {- 5 1}})
      (add (num 4) (sub (num 5) (num 1))))
    
; calc : Expr -> num

(define (calc expr)
  (match expr
    [(num n) n]
    [(add l r) (+ (calc l)(calc r))]
    [(sub l r) (- (calc l)(calc r))]))

; run :: sexpr-v -> number
(define (run prog)
  (calc (parse prog)))

(test (run '{+ 4 2}) 6)
(test (run '3) 3)
(test (run '{- 4 2}) 2)




