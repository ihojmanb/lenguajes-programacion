#lang play

(deftype Expr
  (num n)
  (add l r))

(define (interp expr cont); continuation-passing style
  (match expr
    [(num n) (cont n)]
    [(add l r)
     (interp l (λ(vl)
                 (interp r (λ(vr)
                             (cont (+ vl vr))))))]))


(interp (add (num 10) (num 2)))
---->[interp]----

[(add (num 10) (num 2))
 (interp (num 10) (λ(vl)
                    (interp (num 2) (λ (vr)
                                      (cont (+ vl vr))))))]
------[interp]---------; el lado izquierdo l
[(num 10) ; hay que pasar 10 a (cont n) donde
 ;cont es (λ(vl)(...))
 (λ (10)
   (interp (num 2) (λ (vr)
                     (cont (+ 10 vr)))))
 ]

------[interp]---------; el lado derecho r
[(num 2) ; hay que pasar 2 a (cont n) donde
 ;cont es (λ(vr)(...))
 (λ (2)
   (cont (+ 10 2)))
 ]










