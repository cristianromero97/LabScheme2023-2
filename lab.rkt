#lang racket

;DOM : code (int) x message (string) x chatbotcodelink (int) x flowcodelink (int) x keyword
;REC : list
;Recursion : Cola
;Resumen : Funcion que me permite crear opciones.
(define (op numero1 numero2 numero3 keyword)
  (define (op-aux numero1 numero2 numero3 keyword opciones)
    (let ((nueva-opcion
           (list
             (option numero1)
             (id numero2)
             (flujo numero3)
             (synonim keyword))))
      (cons nueva-opcion opciones)))
  (op-aux numero1 numero2 numero3 keyword '()))

;DOM : numero (int)
;REC : list
;Recursion : Ninguna
;Resumen : Funcion que dependiendo del numero lo unira a un enunciado.
(define (option numero)
  (cond
    ((= numero 1) "1 Viajar")
    ((= numero 2) "2 Estudiar")
    ((= numero 3) "3 Arrendar un auto")
    ((= numero 4) "4 Pedir hora medica")
    (else '())))

;DOM : keyword (string)
;REC : list
;Recursion : Ninguna
;Resumen : Funcion que dependiendo de la palabra clave lo une a sinonimos.
(define (synonim keyword)
  (cond
    ((string=? keyword "viajar") '("viajar" "conocer" "turistear"))
    ((string=? keyword "estudiar") '("estudiar" "aprender" "perfeccionarme"))
    ((string=? keyword "arrendar") '("arrendar" "alquilar" "rentar"))
    ((string=? keyword "pedir")'("pedir" "solicitar" "requerir"))
    (else '())))

;pertenecia
;DOM : numero (int)
;REC : numero
;Recursion : Ninguna
;Resumen : Verifica si una variable es un numero natural.
(define (id numero)
  (if (integer? numero)
      numero
      #f))

;pertenencia
;DOM : numero (int)
;REC : numero
;Recursion : Ninguna
;Resumen : Verifica si una variable es un numero natural.
(define (flujo numero)
  (if (integer? numero)
      numero
      #f))

;Para crear mas opciones se debe seguir el siguiente planteamiento
(define op1 (op 1 2 4 "viajar"))
(define op2 (op 2 4 3 "estudiar"))

; Ejemplo de uso
(define opciones '()) ; Inicializamos la lista de opciones
;(set! opciones (op 1 2 3 "viajar" opciones)) ; Agregamos la primera opción
;(set! opciones (op 2 3 4 "estudiar" opciones)) ; Agregamos la segunda opción

; Función para consultar opciones
(define (consultar-opciones opciones)
  opciones)

;(display (consultar-opciones opciones)) ; Muestra todas las opciones almacenadas

;lista aqui ir colocando todas las opciones
(define opciones-disponibles (list op1 op2))

;DOM : name (string) x option(list)
;REC : flow
;Recursion : Natural
;Resumen : A un flujo determinado le va asignando opciones.
(define (flow tipo cantidad)
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