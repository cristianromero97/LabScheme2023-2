#lang racket
;Definicion de librerias de tiempo
(require racket/date) ;libreria date para fechas
(define fecha(current-date)) ;defino fecha como hora actual

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

;DOM : name (string) x option(list)
;REC : flow
;Recursion : Ninguna
;Resumen : A un flujo determinado le va asignando opciones.
(define (flow tipo . opciones)
  (cons tipo opciones))

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

;DOM : flow x option
;REC : flow
;Recursion : Ninguna
;Resumen : Añade a un flow existente una opcion y lo mete a una nueva lista o flujo (una forma de verla es esta).
(define (flow-add-option-1 lista option)
  (if (member option lista)
    (list option)       
      '()))


;DOM : flow x option
;REC : flow
;Recursion : Ninguna
;Resumen : Añade a un flow existente una opcion y lo mete a una nueva lista o flujo (esta forma es la que la usare).
(define (flow-add-option-2 flow option)
  (if (and (list? flow) (list? option))
      (if (member option (cdr flow))
          flow
          (cons (car flow) (cons option (cdr flow))))
      '()))

;DOM : name (string) x mensaje (string) x flows
;REC : chatbot
;Recursion : Ninguna
;Resumen : Crea un chatbot o bot inicial con ciertos parametros, basandonos en la estructura de listas, se debe asignar un nombre
; un mensaje de bienvenida y la opcion que se va a realizar (ver ejemplo).
(define (chatbot name mensaje L)
  (if (and (string? name)(string? mensaje)(list? L))
     (list
      (cons name (cons mensaje (cons L '())))
      
     )null))

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

;DOM : name (string) x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Funcion constructora de un sistema de chatbots, deja la fecha como registro de cuando se crea. 
(define (system sistema chatbots)
  (if (and (string? sistema) (list? chatbots))
     (list
        (cons sistema (cons chatbots
           (cons (date->string fecha) '() ))))
      null))

;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Función modificadora para añadir chatbots a un sistema. 
(define (system-add-chatbot sistema chatbot)
  (if (and (list? sistema) (list? chatbot))
      (if (member chatbot sistema)
          sistema
          (reverse (cons chatbot (reverse sistema))))
      '()))

;Funciones extra
;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Funcion anexa que intenta ver duplicados. 
(define (remover-duplicado sistema chatbot)
  (if (and (list? sistema) (list? chatbot))
      (let ((chatbot-name (car chatbot)))
        (if (contains-chatbot sistema chatbot-name)
            sistema
            (cons chatbot sistema)))
      '()))
;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Funcion anexa que verifica duplicados contenidos. 
(define (contains-chatbot sistema chatbot-name)
  (cond
    ((null? sistema) #f)
    ((equal? chatbot-name (car sistema)) #t)
    (else (contains-chatbot (cdr sistema) chatbot-name))))

;(define s98 (remover-duplicado (cdr s1) (car s1)))

;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Función que verifica en caso de no haber duplicados. 
(define (system-add-chatbot-no-duplicado sistema chatbot)
  (if (and (list? sistema) (list? chatbot))
      (if (not (contains-chatbot sistema (car chatbot)))
          (cons chatbot sistema)
          sistema)
      '()))

;(define s99 (system-add-chatbot-no-duplicado s1 cb11))

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

;DOM : system X user (string)
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite iniciar una sesión en el sistema.
(define (system-login sistema usuario)
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

;DOM : system
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite cerrar una sesión abierta.
(define (system-logout sistema usuario)
  (if (and (string? usuario) (list? sistema))
      (if (list-member (car sistema) usuario)
          "Se ha cerrado la sesion correctamente."
          "Error: Usuario no esta dentro del sistema.")
      "Error: Argumentos no válidos."))


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

;DOM : system X message (string)
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite interactuar con un chatbot. Mismo propósito de la función anterior pero con una implementación declarativa
(define (system-talk-norec sistema keyword)
  (let loop ((sistema sistema))
    (cond
      ((null? sistema) '("Opción no encontrada")) ; Si no se encuentra en ningún chatbot..
      ((list? sistema)
       (let* ((chatbot (car sistema))
              (resultado (buscar-en-chatbot chatbot keyword)))
         (if (not (null? resultado))
             resultado
             (loop (cdr sistema)))))
      (else (loop (cdr sistema))))))

;DOM : chatbot x string (keyword)
;REC : chatbot
;Recursion : Ninguna
;Resumen : Función que busca al chatbot correspondiente para su recorrido en la funcion anterior.
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


; EJEMPLO DE USO

;Para crear mas opciones se debe seguir el siguiente planteamiento
(define op1 (op 1 2 4 "viajar"))
(define op2 (op 2 4 3 "estudiar"))
(define op3 (op 2 4 2 "estudiar"))
(define op4 (op 4 2 3 "pedir"))

;Ejemplo anexos
(define opciones '()) ; Inicializamos la lista de opciones
;(set! opciones (op 1 2 3 "viajar" opciones)) ; Agregamos la primera opción
;(set! opciones (op 2 3 4 "estudiar" opciones)) ; Agregamos la segunda opción

; Función para consultar opciones
(define (consultar-opciones opciones)
  opciones)

;(display (consultar-opciones opciones)) ; Muestra todas las opciones almacenadas

;lista aqui ir colocando todas las opciones
(define opciones-disponibles (list op1 op2 op3 op4))

;Ejemplo de uso funcion flow
(define f09(flow "flujo1" op1 op2)) ;primera forma de ver los flujos
(define f099(flow-aux "flujo1" 2)) ;segunda forma de ver los flujos

;Ejemplos de uso flow-add-option
(define f10 (flow "flujo1" op1)) ;creacion de otro flujo
(define f11(flow-add-option-2 f10 op1))
(define f12(flow-add-option-2 f11 op2))

;Ejemplo de uso chatbot
(define cb10(chatbot "Asistente"  "Bienvenido ¿Qué te gustaría hacer?"   f12))

;Ejemplo de uso chatbot-add-flow
(define cb11 (chatbot-add-flow cb10 f12))

;Ejemplo de uso system
(define s0(system "newSystem" cb11))

;Ejemplo de uso system-add-chatbot
(define s1 (system-add-chatbot s0 cb11))

;Ejemplo de uso system-add-user
(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
(define s5 (system-add-user s4 "user3"))

;Ejemplo de uso system-login

(define s44 (system-login s3 "user1")) ; Devolverá "Se ha ingresado a sesión correctamente." porque "user0" está en la lista de usuarios.
(define s55 (system-login s3 "user9")) ; Devolverá "No se ha podido ingresar a sesión. El usuario no existe." porque "user9" no está en la lista de usuarios.
(define s66 (system-login s3 42)) ; Devolverá "Error: Argumentos no válidos." porque el usuario no es una cadena.

;Ejemplo de uso system-logout

(define s444 (system-logout s3 "user1"))

;Ejemplo de uso system-talk-rec
(define s6 (system-talk-rec s1 "estudiar"))

;Ejemplo de uso system-talk-norec
(define s7 (system-talk-norec s1 "viajar"))

; Ejemplo de uso system-synthesis
(define s9 (system-add-user s1 "user0"))
(define s10 (system-add-chatbot s9 cb11))
(define s11 (system-synthesis s10 "user0"))

