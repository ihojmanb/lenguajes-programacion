;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 2                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P2.rkt")
(print-only-errors #t)

(test (parse 'True) (bool #t))
(test (parse '(A v False)) (bor (id 'A) (bool #f)))
(test (parse '(C ^ (A v B))) (band (id 'C) (bor (id 'A) (id 'B))))
(test (parse '(with (A True) (A ^ A))) (with 'A (bool #t) (band (id 'A) (id 'A))))

(test empty-env (mtEnv))
(test (extend-env 'a (BoolV 'True) empty-env) (aEnv 'a (BoolV 'True) (mtEnv)))
(test/exn (env-lookup 'a empty-env) "Identificador a no definido")
;; DUDA
(test (env-lookup 'a (aEnv 'f (BoolV 'False) (aEnv 'a (PropV 'A) empty-env))) (PropV 'A)) ;; no se si
;; este ejemplo es correcto. Es corecto definir (PropV 'A)?

;; Test my-or
(test (my-or (BoolV #t) (BoolV #t)) (BoolV #t))
(test (my-or (BoolV #f) (BoolV #t)) (BoolV #t))
(test (my-or (BoolV #t) (BoolV #f)) (BoolV #t))
(test (my-or (BoolV #f) (BoolV #f)) (BoolV #f))
(test/exn (my-or (interp (id 'A) empty-env) (interp (bool #f) empty-env)) "Identificador A no definido")
;; Test my-and
(test (my-and (BoolV #t) (BoolV #t)) (BoolV #t))
(test (my-and (BoolV #f) (BoolV #t)) (BoolV #f))
(test (my-and (BoolV #t) (BoolV #f)) (BoolV #f))
(test (my-and (BoolV #f) (BoolV #f)) (BoolV #f))
(test/exn (my-and (interp (id 'A) empty-env) (interp (bool #f) empty-env)) "Identificador A no definido")

;; Test interp
(test (interp (parse 'True) empty-env) (BoolV #t))
(test/exn (interp (parse '(A ^ False)) empty-env) "Identificador A no definido")
(test/exn (interp (parse '(C ^ (A v B))) empty-env) "Identificador B no definido")
(test (interp (parse '(with (A True) (A ^ False))) empty-env) (BoolV #f))
(test/exn (interp (parse '(A v False)) empty-env)  "Identificador A no definido")
(test (interp (parse '(with (A True) (A ^ A))) empty-env) (BoolV #t))
(test (interp (parse '(A v False)) (aEnv 'A (BoolV #t) empty-env)) (BoolV #t))
