#lang racket
; con identificadores
{with {x 5}
  {with {y 10}
     {+ x y}}}

;indices de "de Bruijn"
{with 5
  {with 10 
   {+ <1> <0>}}}


; con identificadores
{with {x 5}
  {with {y {+ x 1}}
     {+ x y}}}

;indices de "de Bruijn"
{with 5
  {with {+ <0> 1} 
   {+ <1> <0>}}}

;substitucion tiene que incrementar el indice a reemplazar
subst 5 at <0> en
  {with {+ <0> 1} 
   {+ <1> <0>}}

-->
{with {+ 5 1}}
   {+ 5 <0>}}} ;subst 5 at <1> en nested boy


;ejercicios
; 0. definir el lenguaje (AST) con indices
;    - (with-db named-expr body); {with <Expr> <Expr>}
;    - (idx n); <n>

; 2 opciones:
;   a- AST 'fusionado' Expr que incluye todos los nodos
;   b- considerar dos lenguajes distintos (Expr vs ExprDB)

; (opcion 1)
; convert : Expr --> Expr
(test (convert (num 10)) (num 10))
(test (convert (with 'x (num 4) (add (ind 'x) (id 'x))))
      (with-db (num 4) (add (idx 0) (idx 0))))

; (opcion 2)
; convert : Expr --> ExprDB
(test (convert (num 10)) (num-db 10))
(test (convert (with 'x (num 4) (add (ind 'x) (id 'x))))
      (with-db (num-db 4) (add-db (idx 0) (idx 0))))


; 1. convertir una expresion con identificadores
;     a una expresion con indices
; 2. definir el interprete (y substitución) para una
;      expresión con indices

;(tiempo estimado: 1 hora)









