#lang racket

(require "TDA-flow_19800734_RomeroMartinez.rkt")
(require "TDA-Option_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : flow x option
;REC : flow
;Recursion : Ninguna
;Resumen : Añade a un flow existente una opcion y lo mete a una nueva lista o flujo (una forma de verla es esta).
(define (flow-add-option-1 lista option)
  (if (member option lista)
    (list option)       
      '()))


;DOM : flow x option
;REC : flow
;Recursion : Ninguna
;Resumen : Añade a un flow existente una opcion y lo mete a una nueva lista o flujo (esta forma es la que la usare).
;DOM : flow x option
;REC : flow
;Recursion : Ninguna
;Resumen : Añade a un flow existente una opcion y lo mete a una nueva lista o flujo (esta forma es la que la usare).
(define (flow-add-option flow option)
  (if (and (list? flow) (list? option))
      (if (not(member option (cadr flow)))          
          (cons (car flow) (cons (cdr flow) option))
          flow)    
    null ))


;Ejemplos de uso flow-add-option
(define f100 (flow "flujo1" op1)) ;creacion de otro flujo
(define f111(flow-add-option f100 op1))
(define f12(flow-add-option f111 op2))
;Ejemplo scrip de pruebas
(define f11(flow-add-option f10 op1)) 

