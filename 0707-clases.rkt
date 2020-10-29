#lang play

;; agregando self
(define counter-self
  (letrec
      ([self
        (let ([count 0]
              [step 1])
          (let ([methods
                 (list
                  (cons 'inc (λ ()
                               (set! count (+ count step))
                               count))
                  (cons 'dec (λ ()
                               (set! count (- count step))
                               count))
                  (cons 'init (λ (v)
                                (set! count v)))
                  (cons 'reset (λ ()
                                 (self 'init 0)))
                  (cons 'step! (λ (v)
                                 (set! step v))))])
            (λ (msg . args)
              (let ([found (assoc msg methods)])
                (if found
                    (apply (cdr found) args)
                    (error "message not understood: " msg))))))])
    self))
;; self en macro
(defmac (OBJECT ([field fname fval] ...)
                ([method mname args body ...] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname fval] ...)
              (let ([methods (list (cons 'mname (λ args body ...)) ...)])
                (λ (msg . vals)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) vals)
                        (error "message not understood: " msg))))))])
    self))

(defmac (-> o m arg ...)
  (o 'm arg ...))


;; 2. lo que nos gustaria escribir
(define counter%
  (OBJECT
   ([field count 0]
    [field step 1])
   ([method inc ()
            (set! count (+ count step))
            count]
    [method dec ()
            (set! count (- count step))
            count]
    [method init (v) (set! count v)]
    [method reset ()  (-> self init 0)]
    [method step! (v) (set! step v)])))

;; lo mismo para el uso (invocar metodos)
; 1. encoding
(counter% 'dec)
; 3.macro para hacer la transformacion

; 2. uso con sintaxis adecuada
(-> counter% dec)
(-> counter% step! 10)
(-> counter% inc)
;(-> counter% step)  ; falla por que los campos (var. de instancia ) estan
; "fuertemente encapsulados"



; objetos anonimos
(OBJECT () ())

;objetos como parametro

(define (set-step-and-reset-inc c)
  (-> c step! 1)
  (-> c reset)
  (-> c inc))

;fabrica de objetos (fabrica == lambda)
#;(define (make-counter [init-val 0] [init-step 1])
    (OBJECT
     ([field count init-val]
      [field step init-step])
     ([method inc ()
              (set! count (+ count step))
              count]
      [method dec ()
              (set! count (- count step))
              count]
      [method init (v) (set! count v)]
      [method reset ()  (-> self init 0)]
      [method step! (v) (set! step v)]))

    )



;(define c1 (make-counter 10 2))
;(define c2 (make-counter))

;fabrica de objetos (fabrica == objeto)

#;(define counter-factory
    (OBJECT
     ([field default-val 0]
      [field default-step 1])
     ([method create ()
              (make-counter default-val default-step)])))


;(define c3 (-> counter-factory create))


; pasar fabrica como parametro
(define (use-factory f)
  (-> f create))

;(define c4 (use-factory counter-factory))



;; Gracia de OO : enlace dinámico

; dynamic dispatch (lo mas importante de OO)
(define (make-node l r)
  (OBJECT
   ([field left l]
    [field right r])
   ([method sum () (+ (-> left sum) (-> right sum))])))

(define (make-leaf v)
  (OBJECT
   ([field value v])
   ([method sum () value])))

(define t (make-node (make-leaf 10) (make-node (make-leaf 20)
                                               (make-leaf 30))))

(-> t sum)

; todo funciona porque el dispath dinamico induce
; una forma de polimorfismo!
; (-> left sum) funciona independiente de quien es 'left' (ditto para 'right')
;mientras pbviamente 'left' entienda 'sum'

; aqui definimos un objeto anonimo que no tiene nada que ver con nodos/arboles
; pero que entiende 'sum' 
(define t2 (make-node (make-leaf 10) (make-node (OBJECT () ([method sum () (random 100)]))
                                                (make-leaf 30))))

(-> t2 sum)


;; igual con funciones de primera clase
;; (define (map f l) .... (f (first l))....)
; -> esta aplicacion de f es "dynamic dispatch"





(define make-counter2
  (let ([methods
         (list
          (cons 'me (λ (self) self))
          (cons 'inc (λ (self) (self '-write 'count
                                     (+ (self '-read 'count)
                                        (self '-read 'step))) 
                       (self '-read 'count))))])
                
    #|(cons 'dec (λ (self) (set! count (- count step)) count))
          (cons 'init (λ (self v) (set! count v) self))
          (cons 'reset (λ (self) (self 'init 0)))
          (cons 'step! (λ (self v) (set! step v))))])|#
    (λ ([init-val 0] [init-step 1])

      (letrec ([self
                (let ([fields (make-hash (list (cons 'count init-val)
                                               (cons 'step init-step)))])

                  (λ (msg . vals)
                    (match msg
                      ['-read (dict-ref fields (first vals))]
                      ['-write (dict-set! fields  (first vals) (second vals))]
                      [_ (let ([found (assoc msg methods)])
                           (if found
                               (apply (cdr found) (cons self vals))
                               (error "message not understood: " msg)))])))])
        self))))

;syntax for field read
(defmac (? o f)
  (o '-read 'f))

; objetos son simplemente sus valores (fields) y su clase
; obj-class, obj-values
(struct obj (class values))

(define Counter
  (let ([methods
         (list
          (cons 'me (λ (self) self))
          (cons 'inc (λ (self) ((obj-class self) '-write self 'count
                                     (+ ((obj-class self) '-read self 'count)
                                        ((obj-class self) '-read self 'step))) 
                       ((obj-class self) '-read self 'count))))])
    (letrec ([class
                 (λ (msg . vals) ; '-read obj 'count
                                 ; '-invoke obj 'm 10 12
                   (match msg
                     ['-create (let ([fields (make-hash (list (cons 'count 0)
                                                              (cons 'step 1)))])
                                 (obj class fields))]
                     ['-read (dict-ref (obj-values (first vals)) (second vals))]
                     ['-write (dict-set! (obj-values (first vals)) (second vals) (third vals))]
                     ['invoke
                      (let ([found (assoc (second vals) methods)])
                        (if found (apply (cdr found) (cons (first vals) (cddr vals)))
                            (error "message not understood: " msg)))]))])
      class)))
              



