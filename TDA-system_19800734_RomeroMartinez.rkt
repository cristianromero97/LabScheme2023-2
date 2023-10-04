#lang racket
;Definicion de librerias de tiempo
(require racket/date) ;libreria date para fechas
(define fecha(current-date)) ;defino fecha como hora actual
(require "TDA-chatbot-add-flow_19800734_RomeroMartinez.rkt")
(require "TDA-chatbot_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : name (string) x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Funcion constructora de un sistema de chatbots, deja la fecha como registro de cuando se crea. 
(define (system sistema . chatbots)
  (if (and (string? sistema) (list? chatbots))
      (list
       (cons sistema
             (cons (delete-duplicates chatbots)  ; Eliminar duplicados
                   (list (date->string fecha)))))
      null))

;DOM : list
;REC : system
;Recursion : Ninguna
;Resumen : Funcion que elimina duplicados. 
(define (delete-duplicates lst)
  (cond
    ((null? lst) '())
    ((member (car lst) (cdr lst))
     (delete-duplicates (cdr lst)))
    (else
     (cons (car lst) (delete-duplicates (cdr lst))))))


;Ejemplo de uso system
(define s1(system "newSystem" cb11))
;ejemplo scrips de pruebas
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2 ))