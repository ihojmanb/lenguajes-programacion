#lang play
(require "T1.rkt")
(print-only-errors #t)

;;definimos variables para todos los tests
(def frac1 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(def frac2 (simple 4))
(def frac3 (compound 1/3 -1 (simple 3)))

;; b.
(test (eval frac1) 3.245)
(test (eval frac2) 4)
(test (eval frac3) 0)

;; c.

(test (degree frac1) 3)
(test (degree frac2) 0)
(test (degree frac3) 1)

;; d.
(test ((fold identity (λ(i n d) (+ i (/ n d)))) frac1) 3.245)
(test ((fold identity (λ(i n d) (+ i (/ n d)))) frac2) 4)
(test ((fold identity (λ(i n d) (+ i (/ n d)))) frac3) 0)

;; e.
(test (eval2 frac1) 3.245)
(test (eval2 frac2) 4)
(test (eval2 frac3) 0)

(test (degree2 frac1) 3)
(test (degree2 frac2) 0)
(test (degree2 frac3) 1)

;; f.
(test (sqr 0) 0)
(test (sqr -3) 9)
(test (sqr 12) 144)
(test/exn (mysterious-cf -5) "Error: argumento negativo")
(test (mysterious-cf 0) (simple 3))
(test (mysterious-cf 1) (compound 3 1 (simple 6)))
(test (mysterious-cf 2) (compound 3 1 (compound 6 9 (simple 6))))
(test (mysterious-cf 3) (compound 3 1 (compound 6 9 (compound 6 25 (simple 6)))))
(test (mysterious-cf 4) (compound 3 1 (compound 6 9 (compound 6 25 (compound 6 49 (simple 6))))))


;; g.
(test (from-to 0 0) '(0))
(test (from-to 0 3) '(0 1 2 3))
(test (from-to -3 3) '(-3 -2 -1 0 1 2 3))
(test (raw-mysterious-list 0) (list (simple 3)))
(test (raw-mysterious-list 3)
      (list (simple 3) (compound 3 1 (simple 6)) (compound 3 1 (compound 6 9 (simple 6))) (compound 3 1 (compound 6 9 (compound 6 25 (simple 6))))))
(test (mysterious-list 0) '(3.0))
(test (mysterious-list 1) '(3.0 3.1666666666666665))
(test (mysterious-list 2) '(3.0 3.1666666666666665 3.1333333333333333))
(test (mysterious-list 3) '(3.0 3.1666666666666665 3.1333333333333333 3.145238095238095))

;; h.
(test/exn (rac-to-cf (+ -7 15/21)) "Error: argumento negativo")
(test(rac-to-cf (+ 3 49/200))
     (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test(rac-to-cf 5/2) (compound 2 1 (simple 2)))
(test (rac-to-cf 3) (simple 3))
(test (rac-to-cf 0) (simple 0))

