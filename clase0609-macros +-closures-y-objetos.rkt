#lang play
;macros : extensiones sintacticas
;ms :: AST -> AST
;-> al menos el output parsea
;-> no depende de la sintaxis concreta

;Ejemplo: let con un solo binding
(defmac
  (slet [id val] body) ;patron
  ;metavariables: id val vody
  ;(let ([id val]) body))  ; equivalente
  ((λ (id) body) val)) ; template

(slet [x 10]
      (+ x x))
;; --> ((λ (x) (+ x x)) 10)



;timer: no pede ser una funcion
; (las funciones toman sus argumentos  ya evaluados!!)
(defmac (my-time e) ; metavariable: e (patron trivial)
  (let ([begin-time (current-milliseconds)])
    (begin
      e
      (- (current-milliseconds) begin-time))))

(my-time (expt 2 10000000))

; hagan click en macro stepper y ovserven la expansion de slet y my-time
; (asegunrense que el "macro hiding" esta en "standard")

;; 1.Ellipsis

; (mlet ([x 10] [y 20] [z 30]) (+ x y z))
; ---> (λ (x y z) (+ x y z) 10 20 30)
; uso de *ellipsis* para expresar "0" o "mas" de la s-expr que esta antes
(defmac (mlet ([id val] ...) body)
  ((λ (id ...) body) val ...))

(mlet ([x 10] [y 20] [z 30]) (+ x y z))
(mlet () 1)

;; 2.Ellipsis con restriccion de al menos 1

; ahora un version de mlet que exige al menos 1 binding
(defmac (nlet ([id0 val0][id val] ...) body)
  ((λ (id0 id ...) body) val0 val ...))
(nlet ([x 10] [y 20] [z 30]) (+ x y z))
;(nlet () 1) <----- error de sintaxis!


;; 3.Keywords

;podriamos agrgar un else a nuestro lenguaje como keyword

(defmac (cond2 (c e1) (else e2))
  (if c e1 e2))
(cond2 ((> 3 5) (+ 20 10))
       (else 10))

;pero en el caso de arriba, else esta considerado como una meta variable que
;no estamos utilizando y por lo tanto, el siguiente programa tambien funcionaría
(cond2 ((> 3 5) (+ 20 10))
       ((/ 2 0) 10))

; como podemos hacer que else sea una keyword?
(defmac (cond3 (c e1) (else e2))
  #:keywords else
  (if c e1 e2))
;este ejemplo es error de sintaxis si 'else' es un keyword
;(cond3 ((> 3 5) (+ 20 10))
;       ((/ 2 0) 10))


;; 4. PITFALL 1: cuidado con duplicar las expresiones!
(defmac (my-or e1 e2)
  (let ([res e1])  ; sin el let, se evalua e1 dos veces 
    (if res res e2)))

(my-or (begin (print "hola") 2)
       #f)

; 5. HYGIENIC MACROS (o, como preservar scope lexico en un mundo expansible)

; ¿qué pasa si el cliente justo usa el mismo nombre de variable usado por la macro?
(let ([res 10])
  (my-or false res))

;a que expande esto;
; a esto? no
(let ([res 10])
  (let ([res false])
    (if res res res)))
;esto no es lo que queremos, y sería el resultado de un sistema no-higienico (C, Lisp)

;respuesta: expande a esto!
(let ([res 10])
  (let ([res_1 false]) ; en el macro stepper se usan colores en vez de _1, _2, etc.
    (if res_1 res_1 res)))


;; macro para for

(defmac (for0 <var> from <low> to <high> in <body> ...)
  #:keywords from to in
  (let ([high-val <high>]) ;calculamos una vez el valor de <high>
    (letrec ([loop (lambda (<var>)
                     (cond
                       [(<= <var> high-val)
                        (begin
                          <body> ...
                          (loop (add1 <var>)))]))])
      (loop <low>))))

#;(for0 x from 2 to (begin (print "hola") 15) in (display x))

#;(for0 x from 2 to 10 in (display x))


; PITFALL 2: orden de evaluación! 
#;(for0 x from (read) to (read) in (display x))

; en esta version, arreglamos el orde de evaluacion (de izquierda a derecha)
(defmac (for1 <var> from <low> to <high> in <body> ...)
  #:keywords from to in
  (let ([low-val <low>]
        [high-val <high>]) ;calculamos una vez el valor de <high>
    (letrec ([loop (lambda (<var>)
                     (cond
                       [(<= <var> high-val)
                        (begin
                          <body> ...
                          (loop (add1 <var>)))]))])
      (loop low-val))))

#;(for1 x from (read) to (read) in (display x))


;Identificadores implicitos: no funcionan.....

(defmac (for2  from <low> to <high> in <body> ...)
  #:keywords from to in
  (let ([low-val <low>]
        [high-val <high>]) ;calculamos una vez el valor de <high>
    (letrec ([loop (lambda (it)
                     (cond
                       [(<= it high-val)
                        (begin
                          <body> ...
                          (loop (add1 it)))]))])
      (loop low-val))))


(for2 from 1 to 9 in (print "hola"))
#;(for2 from 1 to 9 in (display it)) ; error de sintaxis debido a la higiene


;Identificadores implicitos: captura!
; hay que escapar de la higiene de manera selectiva, parte de la spec de for3

(defmac (for3  from <low> to <high> in <body> ...)
  #:keywords from to in
  #:captures it
  (let ([low-val <low>]
        [high-val <high>]) ;calculamos una vez el valor de <high>
    (letrec ([loop (lambda (it)
                     (cond
                       [(<= it high-val)
                        (begin
                          <body> ...
                          (loop (add1 it)))]))])
      (loop low-val))))

(for3 from 1 to 9 in (display it))













