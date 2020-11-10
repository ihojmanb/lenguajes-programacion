;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 2                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P3.rkt")
(print-only-errors #t)
;; Tests parse
(test (parse 8) (real 8))
(test (parse '(+ 1 (2)i)) (comp 1 2))
(test (parse '(+ 3 6)) (add (real 3) (real 6)))
(test (parse 'x) (id 'x))
(test (parse '(fun (x) (+ 2 x))) (fun (id 'x) (add (real 2) (id 'x))))
(test (parse '(f (+ 2 3))) (app (id 'f) (add (real 2) (real 3))))
(test (parse '((+ y 5) where y = (+ 2 4))) (app (fun (id 'y) (add (id 'y) (real 5))) (add (real 2) (real 4))) )

;; Tests sum+
(test (sum+ (realV 2) (realV 3)) (realV 5))
(test (sum+ (realV 2) (compV 5 1)) (compV 7 1))
(test (sum+ (realV 3) (compV -5 -5)) (compV -2 -5))
(test (sum+ (compV 5 1) (realV -4)) (compV 1 1))
(test (sum+ (compV 3 4) (compV 5 1)) (compV 8 5))
(test (sum+ (compV 0 0) (compV 0 0)) (compV 0 0))