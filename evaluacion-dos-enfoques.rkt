#lang racket
;; enfoque 1: evaluacion perezosa
; -> se substituye la expresion nombrada, sin evaluar
; puede evalluar 0 o n veces la expresión nombrada
;(segun el numero de ocurrencias del id en el cuerpo)
{with {x {+ 10 10}}
      {+ x x}} ; que pasa si en vez de {+ x x} ponemos {3}?
-->[subst] 
{+ {+ 10 10} {+ 10 10}}

-->[calc]*
40

;; enfoque 2: evaluacion temprana
; -> se evalua la expresión nombrada antes de substituir
;siempre evalua 1 sola vez la expresión nombrada
{with {x {+ 10 10}}
      {+ x x}}

--> [calc]
{with {x 20}
      {+ x x}}


-->[subst]
{+ 20 20}

-->[calc]
40

;efecto y orden de evaliacion
{with {x {print 1}}
   {with {y {print 2}}}
   {+ y x}

----eval perezosa----->[subst]
{+{print 2}{print 1}}


; costo
{with {x {prime-factor z n}}; calculo costoso
   {+ x x}}
;con evaluación temprana hago la pega de ese calculo costoso una vez 

;pero si no ocupo el calculo, me conviene la evaluación perezosas
{with {x {prime-factor z n}}; 
   {+ 1 2}}












