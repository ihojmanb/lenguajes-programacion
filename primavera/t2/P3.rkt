;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P3 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO: IANIV HOJMAN
;; Mucho éxito :)

#lang play

#| ================================
                PARTE A
   ================================|#




#| ================================
                PARTE B
   ================================ |#

#|
<s-expr> ::= <num>
           | <sym>
           | (...)
           | (...)
           | (...)
           | (...)
           | (...)  <- syntactical sugar
|#
;; parse :: s-expr -> Expr



#| ================================
                PARTE C
   ================================ |#

;; Values of Expressions
;; <value> ::= ...



;; num+ :: Value Value -> Value




#| ================================
                PARTE D
   ================================ |#

;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value



;; eval :: Expr Env -> Value


;; run :: s-expr -> Value




