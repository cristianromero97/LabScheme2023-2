#lang racket

(require "TDA-system-add-chatbot_19800734_RomeroMartinez.rkt")
(require "TDA-system_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : system X user (string)
;REC : system
;Recursion : Ninguna
;Resumen : . Función modificadora para añadir usuarios a un sistema.
(define (system-add-user sistema usuario)
  (if (string? usuario)
      (if (not (member usuario (car sistema)))
          (cons (cons usuario (car sistema)) (cdr sistema))
          sistema)
      '()))

;Ejemplo de uso system-add-user
(define s6 (system-add-user s121 "user1"))
(define s7 (system-add-user s6 "user2"))
(define s8 (system-add-user s7 "user2")) ;solo añade un ocurrencia de user2
(define s9 (system-add-user s8 "user3"))

;ejemplos scrpis de pruebas
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2"))
(define s5 (system-add-user s4 "user3"))