#lang play
(require "base.rkt")
(print-only-errors #t)
;;;;;;;;;;;;;;;;;;;;;;;;PARSING;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(test (parse '(cons 1 (cons 2 (cons 3 nil))))
      (par (num 1) (par (num 2) (par (num 3) '()))))

#;(test (parse '(match l as (nil => 0) (cons x xs => (+ x 5))))
      (mtch (id 'l) (list (par '() (num 0))
                          (par (par 'x 'xs) (add (id 'x) (num 5))))))

;;;;;;;;;;;;;;;;;;;;;;;;COCIENTE;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (run '(with (halve (fun (n) (/ n 2))) (halve 7)))
      (numV 3))

(test/exn (run '(with (halve (fun (n) (/ n 0)))
                      (halve 8)))
          "quotient: undefined for 0")

;;;;;;;;;;;;;;;;;;;;;;;;LISTAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



