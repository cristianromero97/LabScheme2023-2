#lang racket
(define op1
  (lambda (numero1 numero2 numero3 keyword)
    (list
      (option numero1)(id numero2)(flujo numero3)(synonim keyword))))

(define (option numero)
  (cond
    ((= numero 1) "1 Viajar")
    ((= numero 2) "2 Estudiar")
    ((= numero 3) "3 Arrendar un auto")
    ((= numero 4) "4 Pedir hora medica")
    (else "Saliendo del chat")))

(define (synonim keyword)
  (cond
    ((string=? keyword "viajar") '("viajar" "conocer" "turistear"))
    (else '())))

(define (id numero)
  (if (integer? numero)
      numero
      #f))

(define (flujo numero)
  (if (integer? numero)
      numero
      #f))