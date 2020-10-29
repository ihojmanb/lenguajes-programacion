#lang play
(print-only-errors #t)
;p1
;a

(deftype BinTree
  (leaf val)
  (node left-bt val right-bt))
(define t
  (node (node (leaf 1) 2 (leaf 5)) 20 (leaf 30)))
(define bt0
  (node (leaf 3) 7 (node (leaf 5) 12 (leaf 2))))
(define bt1
  (node (leaf "a") "b" (node (leaf "c") "d" (leaf "e"))))
; map-bt :: (fun arg BinTree) -> BinTree
; aplica la funcion fun con los argumentos arg a cada elemento en el arbol
(define (map-bt f  bt)
  (match bt
   [(node left val right) (node (map-bt f  left) (f  val) (map-bt f  right))]
   [(leaf val) (leaf (f  val))]
  ))

(test (map-bt add1  t) (node (node (leaf 2) 3 (leaf 6)) 21 (leaf 31)))
(test (map-bt number->string bt0)
      (node (leaf "3") "7" (node (leaf "5") "12" (leaf "2"))))




;fold-bt :: (fun x BinTree) -> Val
; aplica fun a todos los elementos de BinTree y combina los valores en uno
(define (fold-bt f t)
  (match t
    [(node l v r) (f(v (fold-bt f l) (fold-bt f r)))]
    [(leaf val) (leaf (f val))]))
(test (fold-bt + t) 53)
(test (fold-bt string-append bt1) "badce")









