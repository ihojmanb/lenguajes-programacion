#lang play
(print-only-errors #t)
;p1
;a
(cons 'a 'b) ; par
(list 'a 'b) ; lista
(cons 'a (cons 'b '())) ;lista en notación de par
;b
'((a b) c)
(list (list 'a 'b) 'c)
;c
(define l
  (list '(a b c) '(d e f) '(g h i)))
(car(cdr (car (cdr l)))); e
(car(cdr (cdr (car l)))); c
; d
(cons 'c '()); '(c)
(cons  'a (cons 'b '())); '(a b)
(cons (cons  'a (cons 'b '())) (cons 'c '())); '((a b) c)

;p2
(define (pair-add1 p)
  (cons(+(car p) 1) (+(cdr p) 1)))
(test (pair-add1 (cons 1 1)) (cons 2 2))

;p3
;a
(define (list-add1 l)
  (map add1 l))
(test (list-add1 (list 1 2 3 4 5 6)) (list 2 3 4 5 6 7))
;b
(define (each-length l)
  (map string-length l))
(test (each-length (list "a" "bb" "ccc")) (list 1 2 3))
;c
(define (sum-all l)
  (foldl + 0 l))
(test (sum-all (list 1 2 3)) 6)
;d
(define (concat-all l)
  (foldl string-append "" l))
(test (concat-all (list "a" "bb" "ccc")) "cccbba")
;e
(define (bigger-than-zero l)
  (filter positive? l))
(test (bigger-than-zero (list -1 -2 3)) (list 3))
;p4
;map :: (fun fun-arg ListofVal) -> (ListofVal )
; aplica fun a todos los elementos de la lista y retorna una lista
(define (map-custom f a l)
  (match l
    [(list) '()]
    [(cons h t) (cons (f a h) (map-custom f a t))]))
(test (map-custom + 1 (list 1 2 3 4 5)) (list 2 3 4 5 6))
(test (map-custom * 1 '()) '())

;foldl :: (fun fun-arg ListofVal) ->Val
;aplica la funcion sobre el conjunto de elementos de la lista, devolviendo
;un valor

;(test (foldl-c + 0 (list 1 2 3 4 5)) 15)
;(test (foldl-c + 1 (list)) 1)
;(test (foldl-c * 1 (list 2 2 2 2 2)) 32)
;(test (foldl-c cons '() '(1 2 3 4)) '(4 3 2 1))



;p5
;<bt> :: = <Val>
;         |(cons <bt> <bt>)

(deftype BinTree
  (leaf n)
  (node l r)
)

; node ? :: (BinTree)

























