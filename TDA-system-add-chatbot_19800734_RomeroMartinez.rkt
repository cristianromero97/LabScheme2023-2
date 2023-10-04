#lang racket

(require "TDA-system_19800734_RomeroMartinez.rkt")
(require "TDA-chatbot-add-flow_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Función modificadora para añadir chatbots a un sistema. 
(define (system-add-chatbot sistema chatbot)
  (if (and (list? sistema) (list? chatbot))
      (if (member chatbot sistema)
          sistema
          (reverse (cons chatbot (reverse sistema))))
      '()))

;Funciones extra
;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Funcion anexa que intenta ver duplicados. 
(define (remover-duplicado sistema chatbot)
  (if (and (list? sistema) (list? chatbot))
      (let ((chatbot-name (car chatbot)))
        (if (contains-chatbot sistema chatbot-name)
            sistema
            (cons chatbot sistema)))
      '()))
;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Funcion anexa que verifica duplicados contenidos. 
(define (contains-chatbot sistema chatbot-name)
  (cond
    ((null? sistema) #f)
    ((equal? chatbot-name (car sistema)) #t)
    (else (contains-chatbot (cdr sistema) chatbot-name))))

;(define s98 (remover-duplicado (cdr s1) (car s1)))

;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Función que verifica en caso de no haber duplicados. 
(define (system-add-chatbot-no-duplicado sistema chatbot)
  (if (and (list? sistema) (list? chatbot))
      (if (not (contains-chatbot sistema (car chatbot)))
          (cons chatbot sistema)
          sistema)
      '()))

;(define s99 (system-add-chatbot-no-duplicado s1 cb11))

;Ejemplo de uso system-add-chatbot
(define s121 (system-add-chatbot s0 cb11))
