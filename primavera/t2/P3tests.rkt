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
(test (parse '(fun (x) (+ 2 x))) (fun 'x (add (real 2) (id 'x))))
(test (parse '(f (+ 2 3))) (app (id 'f) (add (real 2) (real 3))))
(test (parse '((+ y 5) where y = (+ 2 4))) (app (fun 'y (add (id 'y) (real 5))) (add (real 2) (real 4))) )


;; Tests sum+
(test (sum+ (realV 2) (realV 3)) (realV 5))
(test (sum+ (realV 2) (compV 5 1)) (compV 7 1))
(test (sum+ (realV 3) (compV -5 -5)) (compV -2 -5))
(test (sum+ (compV 5 1) (realV -4)) (compV 1 1))
(test (sum+ (compV 3 4) (compV 5 1)) (compV 8 5))
(test (sum+ (compV 0 0) (compV 0 0)) (compV 0 0))
;; Test env
(test empty-env (mtEnv))
(test (env-lookup 'x env1) (realV 2))
(test/exn (env-lookup 'y env1) "error: y no está definido")
(test (env-lookup 'y env2) (compV 4 8))
(test (env-lookup 'z env2) (closureV 'x (id 'x) (mtEnv)))
(test/exn (env-lookup 'f env2) "error: f no está definido")
(test (extend-env 'x (realV 2) empty-env) (aEnv 'x (realV 2) (mtEnv)))
(test (extend-env 'y (compV 2 -1) empty-env) (aEnv 'y (compV 2 -1) (mtEnv)))

;; Test eval
(test (eval (real 8) empty-env) (realV 8))
(test (eval (comp 1 2) empty-env) (compV 1 2))
(test (eval (add (real 3) (real 6)) empty-env) (realV 9))
(test/exn (eval (id 'x) empty-env) "error: x no está definido")
(test (eval (id 'y) (aEnv 'y (compV 3 3) empty-env)) (compV 3 3))
(test (eval (fun (id 'x) (add (real 2) (id 'x))) empty-env)
      (closureV (id 'x) (add (real 2) (id 'x)) (mtEnv)))

(test (eval (app (id 'f) (add (real 2) (real 3)))
            (aEnv 'f (closureV 'x (id 'x) (mtEnv)) (mtEnv)))
      (realV 5))
(test/exn (eval (app (id 'f) (add (real 2) (real 3))) empty-env) "error: f no está definido")

(test (eval
         (app (fun 'y (add (id 'y) (real 5))) (add (real 2) (real 4)))
         empty-env)
        (realV 11))
(test (eval
         (app (fun 'y (add (id 'y) (comp 3 1))) (add (real 1) (real 2)))
         empty-env)
        (compV 6 1))
;; Tests Run
(test (run '8) (realV 8))
(test (run '(+ 1 (2)i)) (compV 1 2))
(test (run '(+ 3 6)) (realV 9))
(test/exn (run 'x) "error: x no está definido")
(test (run '(fun (x) (+ 2 x))) (closureV 'x (add (real 2) (id 'x)) (mtEnv)))
(test/exn (run '(f (+ 2 3))) " error: f no está definido")
(test (run '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) (compV 6 4))
(test (run '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) (compV 6 4))
