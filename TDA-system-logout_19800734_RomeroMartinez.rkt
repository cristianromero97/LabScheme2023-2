#lang racket

(require "TDA-system-add-user_19800734_RomeroMartinez.rkt")
(require "TDA-system-login_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : system
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite cerrar una sesión abierta (otra forma de verlo).
(define (system-logout-1 sistema usuario)
  (if (and (string? usuario) (list? sistema))
      (if (list-member (car sistema) usuario)
          "Se ha cerrado la sesion correctamente."
          "Error: Usuario no esta dentro del sistema.")
      "Error: Argumentos no válidos."))

;DOM : system
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite cerrar una sesión abierta.;DOM : system
(define (system-logout sistema usuario)
  (if (and (string? usuario) (list? sistema))
      (if (not (member usuario (car sistema)))
          "Error usuario no encontrado en el sistema..." 
          (cons (cons (list "Cierre de sesion de :" usuario)
            (car sistema))(cdr sistema)))
      "Error de lectura...."))

;Ejemplo de uso system-logout

(define s444 (system-logout s3 "user1"))