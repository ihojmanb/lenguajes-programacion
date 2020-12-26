#lang play
(print-only-errors #t)
(require "P2.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  TESTS - P2                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Nombre y Apellido: Ianiv Hojman
|#
;; some testing
(define expr1 '(with (f (fun (y) y)) (f (4))))
(test (run expr1) (numV 4))
(define expr2 '(with (x 3)
                     (with (f (fun (y) (+ x y)))
                           (f (4)))))
(test (run expr2) (numV 7))
(define expr3 '(with (x 3)
                     (with (f (fun (y) (+ x y)))
                           (with (x 5) (+ x (f (4)))))))
(test (run expr3) (numV 12))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;a)
(test (run '(seq (+ 1 2) 5)) (numV 5))
;b)
(test (run '(with (x 9)
                  ( if0 (seq (set x 2) (- 3 3))
                        (+ 1 x)
                        (- 14 x))))
      (numV 3))

;c)
(test (run'(with (f (fun (z) (+ 1 z)))
                 (f (8)))) (numV 9))

(define expr4 '(with (f (fun (x y z) (+ (- x y) z)))
                     (f (8 3 7))))
(test (run expr4) (numV 12))

(define expr5 '(with (f (fun (y x z) (seq (set y 10) (+ x z))))
                     (with (a 3)
                           (+ (f (a 8 5)) a))))
(test (run expr5)
      (numV 16))


(define expr6 '(with (f (fun (x y z) (seq (set y 10) (+ x z))))
                     (with (a 3)
                           (+ (f (8 a 5)) a))))

(test (run expr6)
      (numV 16))

(define expr9 '(with (a 3)
                     (with (f (fun (x y z) (seq (set y 10) (+ x z))))
                           (+ (f (8 a 5)) a))))
(test (run expr9)
      (numV 16))

(println "TEST REFEXPR")

(define refexpr '(with (f (fun (x y z) (seq (set y 10) (+ x z))))
                       (with (a 3)
                             (+ (f (8 &a 5)) a))))
(test (run refexpr) (numV 23))

;; refexpr con &a en la primera posicion
(define refexpr2 '(with (f (fun (x y z) (seq (set x 11) (+ y z))))
                        (with (a 7)
                              (+ (f (&a 2 3)) a))))
(test (run refexpr2) (numV 16))

(define refexpr3 '(with (f (fun (x y z) (seq (set x 11) (+ y z))))
                        (with (a 7)
                              (+ (f (a 2 3)) a))))
(test (run refexpr3) (numV 12))

;; funcion sin argumentos
(def no-arg-expr '(with (f (fun () (+ 3 2)))
                        (f ())))
(test (run no-arg-expr) (numV 5))