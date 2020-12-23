#lang play
(print-only-errors #t)

(defmac (switch e
                [case question -> res]
                ...
                [default -> default-res])
  #:keywords case default ->
  (let ([val e])
    (match val
      [(? question) res]
      ...
      [_ default-res]
      )))

(define (my-switch e)
  (let ([v e])
    (match v  
      [(? positive?) 7]
      [(? negative?) 40]
      [(? number?)  1729]
      [_ 8]
      )))


(test (switch (- 10 17)
              [case positive? -> 7]
              [case negative? -> 40]
              [case number? -> 1729]
              [default -> 8]) 40)
(test (switch 4
              [case positive? -> 7]
              [case negative? -> 40]
              [case number? -> 1729]
              [default -> 8]) 7)
(test (switch 0
              [case positive? -> 7]
              [case negative? -> 40]
              [case number? -> 1729]
              [default -> 8]) 1729)




(defmac (right-to-left f a b)
  (let ([e2 b] [e1 a])
    (f e2 e1)))

(def f (λ (x y)
         (begin (print x)
                (print y))))
#;(f 'hola 'chao)
#;(right-to-left f 'hola 'chao)






















#;(defmac (for <id> in <list> do <body>)
  #:keywords for in do
  (letrec ([iter (λ(lst)  
                   (match lst
                     ['() (void)]
                     [(list h t) 
                      (begin
                        ((λ('<id>) body) (first lst))
                        (iter (rest lst)))]))])
    (iter))
  )

(defmac (for <id> in <list> do <body>)
  #:keywords for in do
  (let ([result (map (λ (<id>) <body>) <list>)])
    (void)))




(for x in '(2 3 4 5) do (printf "~a\n" (+ x 2))) ; ; -> 4567

#;( let ( [a 10])
     (begin
       ( for x in ' (2 3 4 5) do ( set! a (+ a x)))
       a)) ; ; -> 24



(define (my-for id lst bodyfun)
  (match lst
    ['() (void)]
    [(list l ...) (begin
                    (bodyfun (first lst))
                    (my-for id (rest lst) bodyfun))]))


(my-for 'x '(2 3 4 5) (λ(x) (printf "~a\n" (+ x 2))))

( let ( [a 10])
   (begin
     ( my-for 'x '(2 3 4 5) (λ (x) ( set! a (+ a x))))
     a))














