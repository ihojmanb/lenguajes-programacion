#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;; Ejemplos del enunciado
(test (run-val '(local
              [(define o (object
                          (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get x) (+ (get y) z)))
                          (method set-x (val) (set x val))
                          (method get-y () (get y))))]
            (seqn
             (send o set-x (+ 1 3))
             (+ (send o sum 3) (send o get-y)))))
      11)

#;(test (run-val
       '(local
            [(define a
               (object
                (method auto-apply (o)
                        (send o apply o))
                (method foo () 5)
                ))
             (define o (send a auto-apply
                             (object
                              (method apply (other) (send other apply2 this))
                              (method apply2 (other) this)
                              (method foo () 42))))]
          (send o foo)))
      42)

#;(test (run-val '(local
              [(define smart-computer (object
                                       (method secret? (something) 42)))
               (define everything (object))
               (define oracle (object : smart-computer))]
               (send oracle secret? everything)))
      42)

#;(run-val '(local
              [(define seller (object
                               (method multiplier () 1)
                               (method price (item-number)
                                       (* item-number (send this multiplier)))))
               (define broker (object : seller
                                      (method multiplier () 2)))]
               (send broker price 3)))

#;(test (run-val '(local
                    ([define x (object
                                (field z 3)
                                (method get () (get z)))]
                     [define y (object : x)])
                  (send y get)))
      3)

#;(test/exn (run-val '(local
                        ([define x (object
                                    (field z 3)
                                    (method get () (get z)))]
                         [define y (object
                                    : x
                                    (method get () (get z)))])
                      (send y get)))
          "field not found")

;; A simple monotone counter
(define counter '(object
                  (field count 0)
                  (method incr () (set count (+ 1 (get count))))
                  (method get () (get count))))

(define (incrs-multiply x y)
  `(seqn
    (send ,y incr)
    (seqn
     (send ,x incr)
     (seqn
      (send ,x incr)
      (* (send ,x get) (send ,y get))
      ))))

#;(test (run-val
       `(local ([define c ,counter])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

#;(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (shallow-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      16)

#;(test (run-val
       `(local ([define c (object : ,counter)])
          (seqn (send c incr)
                (local ([define c2 (deep-copy c)])
                  ,(incrs-multiply 'c 'c2)))))
      6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; TESTS
  #;(test/exn
     (run-val '(local
                 [(define a
                    (object
                     (field x 55)
                     (method get-x () (get x))
                     ))]
                 (send a this)
                 ))
     "... used outside of an object")
  #;(test/exn
     (run-val '(local
                 [(define a
                    (object
                     (field x 55)
                     (method get-x () (get x))
                     ))]
                 (send a (get x))
                 ))
     "... used outside of an object")
  #;(test/exn
     (run-val '(local
                 [(define a
                    (object
                     (field x 55)
                     (method get-x () (get x))
                     ))]
                 (send a (set x y))
                 ))
     "... used outside of an object")
  ;; get 
  (test (run-val '(local
                    [(define a
                       (object
                        (field x 55)
                        (method get-x () (get x))
                        ))]
                    (send a get-x)
                    )) 55)



  ;; field not found
  (test/exn (run-val '(local
                        [(define a
                           (object
                            (field x 55)
                            (method get-x () (get x))
                            (method get-y () (get y))
                            ))]
                        (send a get-y)
                        )) "field not found")
  ;; method not found
  (test/exn (run-val '(local
                        [(define b
                           (object
                            (field x 55)
                            (method get-x () (get x))
                            ))]
                        (send b get-y)
                        )) "method not found")
  ;; set
  (test (run-val '(local
                    [(define a
                       (object
                        (field y 0)
                        (method get-y () (get y))
                        (method set-y (val) (set y val))
                        ))]
                    (seqn (send a set-y 11) (send a get-y))
                    )) 11)
  ;; this
  (test (run-val '(local
                    [(define a
                       (object
                        (field x 55)
                        (field y 0)
                        (method get-me () this)
                        (method foo () 42)
                        ))]
                    (send a get-me)
                    ))(object
                       (list
                        (field 'x (num 55))
                        (field 'y (num 0))
                        (method 'get-me '() (this))
                        (method 'foo '() (num 42))))) 

  ;; sequencia de metodos: send, set, get, y custom methods como sum
  (test (run-val '(local
                    [(define o (object
                                (field x 1)
                                (field y 2)
                                (method sum (z) (+ (get x) (+ (get y) z)))
                                (method set-x (val) (set x val))
                                (method get-y () (get y))))]
                    (seqn
                     (send o set-x (+ 1 3))
                     (+ (send o sum 3) (send o get-y)))))
        11)


  (test
   (run-val '(local
               [(define a
                  (object
                   (field x 55)
                   (method get-x () (get x))
                   (method set-x (val) (set x val))
                   (method auto-apply (o)
                           (send o apply o))
                   (method apply (other) (send other apply2 this))
                   (method apply2 (other) this)
                   (method foo () 42)
                   ))]
               (seqn (send a set-x (+ 1 3))
                     (send a get-x))
               )) 4)
;;BONUS
(test (run-val '(local
              [(define f (fun (x)
                              (+ x x)))]
              (f 5))) 10)

  (test (run-val '(local
                    [(define a
                       (object
                        (field x 55)
                        (method get-x () (get x))
                        (method set-x (val) (set x val))
                        (method auto-apply (o)
                                (send o apply o))
                        (method apply (other) (send other apply2 this))
                        (method apply2 (other) this)
                        (method foo () 42)
                        ))]
                    ;(send a set-x (+ 1 3))
                    (send a auto-apply a)
                    )) (object
                        (list
                         (field 'x (num 55))
                         (method 'get-x '() (get 'x))
                         (method 'set-x '(val) (set 'x (id 'val)))
                         (method
                          'auto-apply
                          '(o)
                          (send 'o 'apply (list (id 'o))))
                         (method
                          'apply
                          '(other)
                          (send 'other 'apply2 (list (this))))
                         (method 'apply2 '(other) (this))
                         (method 'foo '() (num 42)))))

