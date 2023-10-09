#lang racket

(require "TDA-flow-add-option_19800734_RomeroMartinez.rkt")
(require "TDA-flow_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : name (string) x mensaje (string) x flows
;REC : chatbot
;Recursion : Ninguna
;Resumen : Crea un chatbot o bot inicial con ciertos parametros, basandonos en la estructura de listas, se debe asignar un nombre
; un mensaje de bienvenida y la opcion que se va a realizar (ver ejemplo).
(define (chatbot name mensaje . funciones)
  (if (and (string? name) (string? mensaje) (not (null? funciones)))
      (let ((nuevas-funciones (eliminar-repetidos funciones '())))
       (cons (list name mensaje) nuevas-funciones))
      funciones))

;DOM : option x list
;REC : list
;Recursion : cola
;Resumen : Permite eliminar duplicados en la funcion de chatbot
(define (eliminar-repetidos funciones lista)
  (if (null? funciones)
      lista
      (let ((nueva-lista (if (not (member (car funciones) lista))
                             (cons (car funciones) lista)
                             lista)))
        (eliminar-repetidos (cdr funciones) nueva-lista))))




;Ejemplo de uso chatbo
(define cb10(chatbot "Asistente"  "Bienvenido ¿Qué te gustaría hacer?"   f12))
;Ejemplo scrips de pruebas
(define cb0 (chatbot "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10 f10))
(define cb1 (chatbot "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f22 f21 f20))
(define cb2(chatbot "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
