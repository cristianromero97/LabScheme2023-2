#lang racket

(require "TDA-system-add-chatbot_19800734_RomeroMartinez.rkt")
(require "TDA-system-add-user_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : system X message (string)
;REC : system
;Recursion : Cola
;Resumen : Función que permite interactuar con un chatbot.
(define (system-talk-rec sistema keyword)
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

  (define (recursivo sistema keyword)
    (cond
      ((null? sistema) '("Opción no encontrada")) ; Si no se encuentra en ningún chatbot
      ((list? sistema)
       (let ((resultado (buscar-en-chatbot (car sistema) keyword)))
         (if (not (null? resultado))
             resultado
             (recursivo (cdr sistema) keyword))))
      (else (recursivo (cdr sistema) keyword))))

  (let ((resultado (recursivo sistema keyword)))
    (if (equal? resultado '("Opción no encontrada"))
        resultado
        (list
        "Hola\n"        
        "Interesante..... entonces planeas" (car resultado)
        "Segun la base de datos tu proposito es" (cdr resultado)
        "Por ende tu objetivo analizando lo anterior seria" (cddr resultado)
        "Espero haberte ayudado..............."
        )
        )))

;Ejemplo de uso system-talk-rec
(define s62 (system-talk-rec s85 "estudiar"))
