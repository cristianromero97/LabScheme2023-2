#lang racket

(require "TDA-system-add-chatbot_19800734_RomeroMartinez.rkt")
(require "TDA-system-add-user_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : system X message (string)
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite interactuar con un chatbot. Mismo propósito de la función anterior pero con una implementación declarativa
(define (system-talk-norec sistema keyword)
  (let loop ((sistema sistema))
    (cond
      ((null? sistema) '("Opción no encontrada")) ; Si no se encuentra en ningún chatbot..
      ((list? sistema)
       (let* ((chatbot (car sistema))
              (resultado (buscar-en-chatbot chatbot keyword)))
         (if (not (null? resultado))
             resultado
             (loop (cdr sistema)))))
      (else (loop (cdr sistema))))))

;DOM : chatbot x string (keyword)
;REC : chatbot
;Recursion : Ninguna
;Resumen : Función que busca al chatbot correspondiente para su recorrido en la funcion anterior.
(define (buscar-en-chatbot chatbot keyword)
  (cond
    ((null? chatbot) '()) ; Si no se encuentra en el chatbot actual
    ((list? (car chatbot))
     (let ((resultado (buscar-en-chatbot (car chatbot) keyword)))
       (if (not (null? resultado))
           resultado
           (buscar-en-chatbot (cdr chatbot) keyword))))
    ((string? (car chatbot))
     (if (string=? (car chatbot) keyword)
         chatbot ; Si se encuentra la opción, se devuelve el chatbot actual
         (buscar-en-chatbot (cdr chatbot) keyword)))
    (else (buscar-en-chatbot (cdr chatbot) keyword))))

;Ejemplo de uso system-talk-norec
(define s72 (system-talk-norec s85 "viajar"))
