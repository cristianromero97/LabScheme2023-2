#lang racket
(require "TDA-system-login_19800734_RomeroMartinez.rkt")
(require "TDA-system-add-chatbot_19800734_RomeroMartinez.rkt")
(require "TDA-chatbot-add-flow_19800734_RomeroMartinez.rkt")
(require "TDA-system-add-user_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM :system X usuario (String)
;REC : string
;Recursion : Ninguna
;Resumen : Función que ofrece una síntesis del chatbot para un usuario particular  a partir de chatHistory contenido dentro del sistema.
(define (system-synthesis sistema usuario)
  ; Verificar que el usuario esté dentro del sistema y haya iniciado sesión
  (let ((logged-in (system-login sistema usuario)))
    (if (string=? logged-in "Se ha ingresado a sesión correctamente.")
        (let ((user-flow (find-user-flow sistema usuario)))
          (if (not (null? user-flow))
              (list "Contenido de la función:" user-flow)
              "El usuario no tiene un flujo asignado."))
        "El usuario no ha iniciado sesión o no está dentro del sistema.")))

;DOM : system x user (String)
;REC : system
;Recursion : Ninguna
;Resumen : Función que busca al usuario si esta dentro del sistema.
(define (find-user-flow sistema usuario)
  ; Buscar el flujo del usuario en el sistema
  (if (null? sistema)
      '()
      (let ((current-user (car sistema)))
        (if (string=? (car current-user) usuario)
            (cdr current-user)
            (find-user-flow (cdr sistema) usuario)))))

; Ejemplo de uso system-synthesis
(define s955 (system-add-user s85 "user0"))
(define s105 (system-add-chatbot s955 cb11))
(define s115 (system-synthesis s105 "user0"))

