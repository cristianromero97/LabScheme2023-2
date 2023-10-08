#lang racket

(require "TDA-system-add-chatbot_19800734_RomeroMartinez.rkt")
(require "TDA-system-add-user_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : system X user (string)
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite iniciar una sesión en el sistema (otra forma de verlo).
(define (system-login-1 sistema usuario)
  (if (and (string? usuario) (list? sistema))
      (if (list-member (car sistema) usuario)
          "Se ha ingresado a sesión correctamente."
          "Error: Usuario no encontrado.")
      "Error: Argumentos no válidos."))

;DOM : lst x item
;REC : list
;Recursion : Ninguna
;Resumen : Función que verifica en caso de haber duplicados. 
(define (list-member lst item)
  (cond
    ((null? lst) #f)
    ((equal? (car lst) item) #t)
    (else (list-member (cdr lst) item))))

;DOM : system X user (string)
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite iniciar una sesión en el sistema.
(define (system-login sistema usuario)
  (if (and (string? usuario) (list? sistema))
      (if (not (member usuario (car sistema)))
          "Error usuario no encontrado en el sistema..." 
          (cons (cons (list "Inicio de sesion de usuario :" usuario)
            (car sistema))(cdr sistema)))
      "Error de lectura...."))


;Ejemplo de uso system-login

(define s44 (system-login s3 "user1")) ; Devolverá "Se ha ingresado a sesión correctamente." porque "user0" está en la lista de usuarios.
(define s55 (system-login s3 "user9")) ; Devolverá "No se ha podido ingresar a sesión. El usuario no existe." porque "user9" no está en la lista de usuarios.
(define s66 (system-login s3 42)) ; Devolverá "Error: Argumentos no válidos." porque el usuario no es una cadena.
