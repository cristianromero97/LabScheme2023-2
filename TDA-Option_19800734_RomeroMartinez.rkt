#lang racket

(provide(all-defined-out))

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
    ((= numero 5) "1 new york")
    ((= numero 6) "3 torres del paine")
    ((= numero 7) "2 paris")
    ((= numero 8) "4 volver")
    ((= numero 9) "1 solo")
    ((= numero 10) "2 museos")
    ((= numero 11) "3 ningun otro atractivo")
    ((= numero 12) "2 en pareja")
    ((= numero 13) "3 en familia")
    ((= numero 14) "5 en realidad quiero otro destino")
    ((= numero 15) "4 agregar mas atractivos")
    ((= numero 16) "4 cambiar destino")
    ((= numero 17) "1 carrera tecnica")
    ((= numero 18) "2 postgrado")
    ((= numero 19) "3 volver")
    ((= numero 20) "1 central park")
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
    ((string=? keyword "new york")'("USA" "Estados Unidos" "new york"))
    ((string=? keyword "torres del paine")'("Chile" "Torres" "Torres del Paine"))
    ((string=? keyword "paris")'("Francia" "Paris" "Eiffel"))
    ((string=? keyword "volver")'("salir" "volvor" "retornar"))
    ((string=? keyword "central park")'("central park" "central park" "central park"))
    ((string=? keyword "solo")'("solo"))
    ((string=? keyword "museos")'("museo"))
    ((string=? keyword "ningun otro atractivo")'("museo"))
    ((string=? keyword "en pareja")'("pareja"))
    ((string=? keyword "en familia")'("familia"))
    ((string=? keyword "en realidad quiero otro destino")'("cambiar destino"))
    ((string=? keyword "agregar mas atractivos")'("volver" "atractivos"))
    ((string=? keyword "cambiar destino")'("museo"))
    ((string=? keyword "carrera tecnica")'("tecnica"))
    ((string=? keyword "postgrado")'("doctorado" "magister" "postgrado"))
    ((string=? keyword "volver")'("volver" "salir" "regresar"))
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

;Ejemplos de uso para las opcionese y como crearlas

;Para crear mas opciones se debe seguir el siguiente planteamiento
(define op1 (op 1 1 1 "viajar"))
(define op2 (op 2 2 1 "estudiar"))
(define op3 (op 5 1 2 "new york" ))
(define op4 (op 7 1 1 "paris" ))
(define op5 (op 6 1 1 "torres del paine" ))
(define op6 (op 8 0 1 "volver" ))
(define op7 (op 20 1 2 "central park" ))
(define op8 (op 10 1 2 "museos" ))
(define op9 (op 11 1 3 "ningun otro atractivo"))
(define op10 (op 16 1 1 "cambiar destino" ))
(define op11 (op 9 1 3 "solo" ))
(define op12 (op 12 1 3 "en pareja" ))
(define op13 (op 13 1 3 "en familia" ))
(define op14 (op 15 1 2 "agregar mas atractivos" ))
(define op15 (op 14 1 1 "en realidad quiero otro destino" ))
(define op16 (op 17 2 1 "carrera tecnica" ))
(define op17 (op 18 2 1 "postgrado" ))
(define op18 (op 19 0 1 "volver" ))

;lista aqui ir colocando todas las opciones;lista aqui ir colocando todas las opciones
(define opciones-disponibles (list op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 op12 op13 op14 op15 op16 op17 op18))