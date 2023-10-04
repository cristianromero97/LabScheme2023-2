#lang racket

(require "TDA-Option_19800734_RomeroMartinez.rkt")
(provide(all-defined-out))

;DOM : name (string) x option(list)
;REC : flow
;Recursion : Ninguna
;Resumen : A un flujo determinado le va asignando opciones.
(define (flow tipo . opciones)
  (define (remove-duplicates lst)
    (if (null? lst)
        '()
        (cons (car lst) (remove-duplicates (filter (lambda (x) (not (equal? (car x) (car (car lst))))) (cdr lst))))))

  (if (and (string? tipo) (list? opciones))
      (list tipo (remove-duplicates opciones))
      '()))


;DOM : name (string) x option(list)
;REC : flow
;Recursion : Natural
;Resumen : A un flujo determinado le va asignando opciones (otra forma de ver la funcion flow).
(define (flow-aux tipo cantidad)
  (define (seleccionar-opciones cantidad opciones)
    (if (= cantidad 0)
        '()
        (cons (car opciones) (seleccionar-opciones (- cantidad 1) (cdr opciones)))))

  (let ((opciones-seleccionadas (seleccionar-opciones cantidad opciones-disponibles)))
    (cons tipo opciones-seleccionadas)))

;pertenencia
;DOM : clave (string)
;REC : string
;Recursion : Ninguna
;Resumen : Verifica si una variable es un string.
(define (clase clave)
  (if(string=? clave)
     clave
  #f)
)

;Ejemplo de uso funcion flow

(define f09(flow "flujo1" op1 op2)) ;primera forma de ver los flujos
(define f099(flow-aux "flujo1" 2)) ;segunda forma de ver los flujos
;Ejemplos del scrips de pruebas
(define f10(flow "Flujo principal Chatbot Bienvenido ¿Que te gustaria hacer?" op1 op2 op2 op2 op1))
(define f20(flow "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f21(flow "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f22(flow "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
(define f30(flow "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))