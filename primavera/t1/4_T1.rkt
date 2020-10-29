#lang play
(require math/flonum)
(print-only-errors #t)
#|
Complete sus datos personales:
NOMBRE Y APELLIDO: Ianiv Hojman
RUT:19245895-1
|#

;; Parte a) ;; REVISAR
;; CFraction :=  <simple <Int> >
;;           |  <Compound <Int> <Int> <Compound> >

(deftype CFraction
  (simple i)
  (compound i numer denom))

;;definimos variables para todos los problemas
(def frac1 (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(def frac2 (simple 4))
(def frac3 (compound 1/3 -1 (simple 3)))


;; Parte b)
;; eval :: CFraction -> Rational
(define (eval frac)
  (match frac
    [(simple i) i]
    [(compound i n d) (+ i (/ n (eval d)))]
    ))


(test (eval frac1) 3.245)
(test (eval frac2) 4)
(test (eval frac3) 0)

;; Parte c)
;; degree ::  CFraction -> Integer
(define (degree frac)
  (match frac
    [(simple i) 0]
    [(compound i n d) (add1 (degree d))]))


(test (degree frac1) 3)
(test (degree frac2) 0)
(test (degree frac3) 1)

;; Parte d)
;; fold :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
(define (fold f g)
  (λ(frac)
    (match frac
      [(simple i) (f i)]
      [(compound i n d) (g i n ((fold f g) d))])))

(test ((fold identity (λ(i n d) (+ i (/ n d)))) frac1) 3.245)
(test ((fold identity (λ(i n d) (+ i (/ n d)))) frac2) 4)
(test ((fold identity (λ(i n d) (+ i (/ n d)))) frac3) 0)
;; Parte e)
;; eval2 :: CFraction -> Rational
(define eval2
  (fold identity (λ(i n d) (+ i (/ n d)))))
(test (eval2 frac1) 3.245)
(test (eval2 frac2) 4)
(test (eval2 frac3) 0)
;; degree2 ::  CFraction -> Integer
(define degree2
  (fold (λ(i) (* 0 1)) (λ(i n d) (add1 d))))
(test (degree2 frac1) 3)
(test (degree2 frac2) 0)
(test (degree2 frac3) 1)

;; Parte f)
;; mysterious-cf :: Integer -> CFraction

;; funcion auxiliar sqr Integer  -> integer
(define (sqr i)
  (* i i))

(define (mysterious-cf k)
  (if (equal? k 0)
      (simple 3)
      (get-mysterious-cf k k))
  )

(define (get-mysterious-cf k j)
  (if (< k 0)
      (error "Error: argumento negativo") 
      (if (equal? k 0)
          (simple 6)
          (if (equal? j k) ;; j = k -> solo la primera vez
              (compound 3 (sqr (- (* 2 (- j (- k 1))) 1))
                        (if (equal? 0 (sub1 k))
                            (simple 6)
                            (get-mysterious-cf (sub1 k) j)))
              (compound 6 (sqr (- (* 2 (- j (- k 1))) 1))
                        (if (equal? 0 (sub1 k))
                            (simple 6)
                            (get-mysterious-cf (sub1 k) j)))
              )
          )
      )
  )
  
(test/exn (mysterious-cf -5) "Error: argumento negativo")
(test (mysterious-cf 0) (simple 3))
(test (mysterious-cf 1) (compound 3 1 (simple 6)))
(test (mysterious-cf 2) (compound 3 1 (compound 6 9 (simple 6))))
(test (mysterious-cf 3) (compound 3 1 (compound 6 9 (compound 6 25 (simple 6)))))
(test (mysterious-cf 4) (compound 3 1 (compound 6 9 (compound 6 25 (compound 6 49 (simple 6))))))


;; Parte g)
;; from-to :: Integer -> Integer -> listOf Integer
(define (from-to i f)
  (cond
    [(< i f) (cons i (from-to (add1 i) f))]
    [(equal? i f) (cons i f)])
  )
(test (from-to 0 3) '(0 1 2 3))
;; mysterious-list :: Integer -> listOf Float

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?



;; Parte h)
;; rac-to-cf :: Rational -> CFraction











