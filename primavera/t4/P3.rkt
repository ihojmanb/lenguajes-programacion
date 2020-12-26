;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  BASE - P3                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Nombre y Apellido: Ianiv Hojman
|#
#lang play
(require math/matrix)
(print-only-errors #t)



;(defmac equilibrium )


( define mouse
   (equilibrium
    [run : (0 -> run)
         (1/2 -> eat)
         (1/2 -> sleep)]
    [eat: (1/4 -> run)
          (0 -> eat)
          (3/4 -> sleep)]
    [sleep : (1/3 -> run)
           (2/3 -> eat)
           (0 -> sleep)]))
