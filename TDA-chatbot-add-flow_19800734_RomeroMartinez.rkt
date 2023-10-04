#lang racket

(require "TDA-chatbot_19800734_RomeroMartinez.rkt")
(require "TDA-flow-add-option_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : chatbot x flows
;REC : chatbot
;Recursion : Cola
;Resumen : Añade lo obtenido en la funcion anterior (chatbot) a un nuevo bot con nuevas opciones. 
(define (chatbot-add-flow flow option)
  (define (add-option-cola flow option result)
    (if (null? flow)
        (reverse (cons option result))
        (if (equal? (car flow) option)
            (reverse (cons option result))
            (add-option-cola (cdr flow) option (cons (car flow) result)))))
  (if (and (list? flow) (list? option))
      (if (member option flow)
          flow
          (add-option-cola flow option '()))
      '()))

;Ejemplo de uso chatbot-add-flow
(define cb11 (chatbot-add-flow cb10 f12))
