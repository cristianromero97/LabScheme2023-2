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
    ((string=? keyword "volver")'("salir" "volver" "retornar"))
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
(define (flow-add-option flow option)
  (if (and (list? flow) (list? option))
      (if (not(member option (cadr flow)))          
          (cons (car flow) (cons (cdr flow) option))
          flow)    
    null ))


;DOM : name (string) x mensaje (string) x flows
;REC : chatbot
;Recursion : Ninguna
;Resumen : Crea un chatbot o bot inicial con ciertos parametros, basandonos en la estructura de listas, se debe asignar un nombre
; un mensaje de bienvenida y la opcion que se va a realizar (ver ejemplo).
(define (chatbot name mensaje . funciones)
  (if (and (string? name) (string? mensaje) (not (null? funciones)))
      (let ((nuevas-funciones (eliminar-repetidos funciones '())))
       (cons (list name mensaje) nuevas-funciones))
      funciones))

;DOM : option x list
;REC : list
;Recursion : cola
;Resumen : Permite eliminar duplicados en la funcion de chatbot
(define (eliminar-repetidos funciones lista)
  (if (null? funciones)
      lista
      (let ((nueva-lista (if (not (member (car funciones) lista))
                             (cons (car funciones) lista)
                             lista)))
        (eliminar-repetidos (cdr funciones) nueva-lista))))

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
(define (system sistema . chatbots)
  (if (and (string? sistema) (list? chatbots))
      (list
       (cons sistema
             (cons (delete-duplicates chatbots)  ; Eliminar duplicados
                   (list (date->string fecha)))))
      null))

;DOM : list
;REC : system
;Recursion : Ninguna
;Resumen : Funcion que elimina duplicados. 
(define (delete-duplicates lst)
  (cond
    ((null? lst) '())
    ((member (car lst) (cdr lst))
     (delete-duplicates (cdr lst)))
    (else
     (cons (car lst) (delete-duplicates (cdr lst))))))


;DOM : system x chatbot
;REC : system
;Recursion : Ninguna
;Resumen : Función modificadora para añadir chatbots a un sistema. 
(define (system-add-chatbot sistema . chatbots)
  (if (and (list? sistema) (list? chatbots))
      (let ((nuevos-chatbots (eliminar-duplicados chatbots (cdr sistema))))
        (if (null? nuevos-chatbots)
            sistema
            (cons (car sistema) nuevos-chatbots)))
      '()))


;DOM : chatbot x list
;REC : chatbots
;Recursion : Ninguna
;Resumen : Funcion que elimina duplicados. 
(define (eliminar-duplicados chatbots lista)
  (if (null? chatbots)
      lista
      (let ((nuevo-chatbot (car chatbots)))
        (if (not (chatbot-existe-en-lista nuevo-chatbot lista))
            (eliminar-duplicados (cdr chatbots) (append lista (list nuevo-chatbot)))
            (eliminar-duplicados (cdr chatbots) lista)))))
;DOM : chatbot x list
;REC : system
;Recursion : Ninguna
;Resumen : Funcion que verifica existencia de chatbots en la lista. 
(define (chatbot-existe-en-lista chatbot lista)
  (if (null? lista)
      #f
      (let ((element (car lista)))
        (if (equal? (car element) (car chatbot))
            #t
            (chatbot-existe-en-lista chatbot (cdr lista))))))

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
        "Por ende tu objetivo analizando lo anterior seria"(cdr resultado)
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


;Ejemplo anexos
(define opciones '()) ; Inicializamos la lista de opciones
;(set! opciones (op 1 2 3 "viajar" opciones)) ; Agregamos la primera opción
;(set! opciones (op 2 3 4 "estudiar" opciones)) ; Agregamos la segunda opción

; Función para consultar opciones
(define (consultar-opciones opciones)
  opciones)

;(display (consultar-opciones opciones)) ; Muestra todas las opciones almacenadas

;lista aqui ir colocando todas las opciones
(define opciones-disponibles (list op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 op12 op13 op14 op15))

;Ejemplo de uso funcion flow
(define f09(flow "flujo1" op1 op2)) ;primera forma de ver los flujos
(define f099(flow-aux "flujo1" 2)) ;segunda forma de ver los flujos

(define f10(flow "Flujo principal Chatbot Bienvenido ¿Que te gustaria hacer?" op1 op2 op2 op2 op1))
(define f11(flow-add-option f10 op1)) 

(define cb0 (chatbot "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10 f10)) 

(define f20(flow "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f21(flow "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f22(flow "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))

(define cb1 (chatbot "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f22 f21 f20))

(define f30(flow "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define cb2(chatbot "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))


(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2 ))
(define s1 (system-add-chatbot s0 cb2 )) 

;Ejemplos de uso flow-add-option
(define f100 (flow "flujo1" op1)) ;creacion de otro flujo
(define f111(flow-add-option f100 op1))
(define f122(flow-add-option f111 op2))


(define s2 (system-add-user s1 "user1"))
(define s3 (system-add-user s2 "user2"))
(define s4 (system-add-user s3 "user2"))
(define s5 (system-add-user s4 "user3"))

(define s6 (system-login s5 "user1"))
(define s7 (system-login s6 "user1"))
(define s8 (system-login s7 "user2"))
(define s9 (system-logout s8 "user2"))
(define s10 (system-login s9 "user2"))



;Ejemplo de uso chatbot
;(define cb100(chatbot "Asistente"  "Bienvenido ¿Qué te gustaría hacer?"   f122))

;Ejemplo de uso chatbot-add-flow
;(define cb111 (chatbot-add-flow cb100 f111))

;Ejemplo de uso system
;(define s0(system "newSystem" cb11))

;Ejemplo de uso system-add-chatbot
;(define s1 (system-add-chatbot s0 cb11))

;Ejemplo de uso system-add-user
;(define s2 (system-add-user s1 "user1"))
;(define s3 (system-add-user s2 "user2"))
;(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
;(define s5 (system-add-user s4 "user3"))

;Ejemplo de uso system-login

;(define s44 (system-login s3 "user1")) ; Devolverá "Se ha ingresado a sesión correctamente." porque "user0" está en la lista de usuarios.
;(define s55 (system-login s3 "user9")) ; Devolverá "No se ha podido ingresar a sesión. El usuario no existe." porque "user9" no está en la lista de usuarios.
;(define s66 (system-login s3 42)) ; Devolverá "Error: Argumentos no válidos." porque el usuario no es una cadena.

;Ejemplo de uso system-logout

;(define s444 (system-logout s3 "user1"))

;Ejemplo de uso system-talk-rec
;(define s6 (system-talk-rec s1 "estudiar"))

;Ejemplo de uso system-talk-norec
;(define s7 (system-talk-norec s1 "viajar"))

; Ejemplo de uso system-synthesis
;(define s9 (system-add-user s1 "user0"))
;(define s10 (system-add-chatbot s9 cb11))
;(define s11 (system-synthesis s10 "user0"))




