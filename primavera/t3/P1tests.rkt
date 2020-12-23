;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 3                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P1.rkt")
(print-only-errors #t)
;;;;;;;;;;;;;;;;;;;;;;;;COCIENTE;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (run '(with (halve (fun (n) (/ n 2))) (halve 7)))
      (numV 3))
(test (run '(with (halve (fun (n) (/ n 2))) (halve 1)))
      (numV 0))
(test (run '(with (halve (fun (n) (/ n 2))) (halve -8)))
      (numV -4))
(test/exn (run '(with (halve (fun (n) (/ n 0)))
                      (halve 8)))
          "quotient: undefined for 0")

;;;;;;;;;;;;;;;;;;;;;;;;PARSE-CASE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def patterns (list '(nil => 0) '(cons x xs => (+ x 5))))
(def (par h t)  (par (num 1) (par (num 2) (par (num 3) (vacio)))))

(test ((parse-case (first patterns))) (num 0))
(test ((second (parse-case (second patterns))) h t) (add (id 'x) (num 5)))

(def patterns2 (list '(nil => 100) '(cons y ys => y)))
(test ((parse-case (first patterns2))) (num 100))
(test ((second (parse-case (second patterns2))) h t) (id 'y))


(def patterns3 (list '(nil => 0) '(cons x xs => (match x as
                                    (nil => 1)
                                    (cons y ys => y)))))
(test ((parse-case (first patterns3))) (num 0))
(test ((second (parse-case (second patterns2))) h t) (id 'y))

;;;;;;;;;;;;;;;;;;;;;;;;check-patterns;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (check-patterns (list '(nil => 0) '(cons x xs => (+ x 5)))) #t)
(test (check-patterns (list '(cons x xs => (+ x 5)) '(nil => 0))) #f)
(test (check-patterns (list '(nil => 0))) #f)
(test (check-patterns (list '(cons x xs => (+ x 5)))) #f)
(test (check-patterns (list)) #f)

;;;;;;;;;;;;;;;;;;;;;;;;LISTAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/exn (run '(with (l (cons 1 (cons 2 (cons 3 nil))))
              (match l as
                (cons x xs => (+ x 5))
                (nil => 0)
                )))
      "match: tienes un error con el orden de patrones")

(test/exn (run '(with (l (cons 1 (cons 2 (cons 3 nil))))
              (match l as
                (cons x xs => (+ x 5))
                )))
      "match: tienes un error con numero de patrones")

(test/exn (run '(with (l (cons 1 (cons 2 (cons 3 nil))))
              (match l as
                (nil => 0)
                )))
      "match: tienes un error con numero de patrones")

(test (run '(with (l (cons 1 (cons 2 (cons 3 nil))))
                  (match l as
                    (nil => 0)
                    (cons x xs => (+ x 5)))))
      (numV 6))

(test (run '(with (l (cons 1 (cons (/ 2 0) nil)))
                  (match l as
                    (nil => 0)
                    (cons y ys => (+ 10 y)))))
      (numV 11))


(test (run '(with (l (cons (/ 1 0) (cons (/ 2 0) nil)))
                  (match l as
                    (nil => 0)
                    (cons z zs => 1))))
      (numV 1))

(test (run '(with (l (cons (+ 1 2) (cons 3 nil)))
                  (match l as
                    (nil => 0)
                    (cons y ys => y))))
      (exprV (add (num 1) (num 2)) (mtEnv) '#&#f))

(test (run '(with (x 1)
                  (with (l (cons (+ 5 x) (cons 200 nil)))
                        (match l as
                          (nil => 0)
                          (cons x xs => (+ 10 x))))))
      (numV 16))

(test (run '(with (l (cons (cons 1 (cons (/ 2 0) nil))
                           (cons (cons 3 nil) nil)))
                  (match l as
                    (nil => 0)
                    (cons x xs => (match x as
                                    (nil => 1)
                                    (cons y ys => y))))))
      (exprV (num 1) (mtEnv) '#&#f))




