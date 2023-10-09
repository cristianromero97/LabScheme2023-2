#lang racket
;Definicion de librerias de tiempo
(require racket/date) ;libreria date para fechas
(define fecha(current-date)) ;defino fecha como hora actual
(provide(all-defined-out))
;Aclaracion: Este archivo posee todos los TDAs, ejemplos y scrips de pruebas (todos los archivos estan separados segun lo
;indicado, el scrips de pruebas y ejemplos se encuentran en un archivo por separado).....
;------------------------------------------------------------------------------------------------------------------------

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

;DOM : system X user (string)
;REC : system
;Recursion : Ninguna
;Resumen : . Función modificadora para añadir usuarios a un sistema.
(define (system-add-user sistema usuario)
  (if (string? usuario)
      (if (not (member usuario (car sistema)))
          (cons (cons usuario
           (car sistema)) (cdr sistema))
          sistema)
      '()))

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

;DOM : system
;REC : system
;Recursion : Ninguna
;Resumen : Función que permite cerrar una sesión abierta.
(define (system-logout sistema usuario)
  (if (and (string? usuario) (list? sistema))
      (if (not (member usuario (car sistema)))
          "Error usuario no encontrado en el sistema..." 
          (cons (cons (list "Cierre de sesion de :" usuario)
            (car sistema))(cdr sistema)))
      "Error de lectura...."))

;DOM : system X message (string)
;REC : system
;Recursion : Cola
;Resumen : Función que permite interactuar con un chatbot.
(define (system-talk-rec sistema keyword)
  ; Función para buscar un flujo específico en el sistema
  (define (buscar-flujo sistema keyword)
    (cond
      ((null? sistema) '()) ; Si no se encuentra en ningún chatbot
      ((list? sistema)
       (let ((resultado (buscar-en-chatbot (car sistema) keyword)))
         (if (not (null? resultado))
             resultado
             (buscar-flujo (cdr sistema) keyword))))
      (else (buscar-flujo (cdr sistema) keyword))))

  ; Función para seleccionar una opción en un flujo dado
  (define (seleccionar-opcion flujo numero)
    (cond
      ((null? flujo) '("Opción no encontrada")) ; Si no se encuentra en el flujo
      ((list? flujo)
       (if (= numero 0) flujo ; Si el número es 0, se devuelve el flujo actual
           (let ((opcion (car flujo)))
             (if (equal? (id opcion) numero)
                 (synonim (synonim keyword) (option opcion))
                 (seleccionar-opcion (cdr flujo) numero))))))
       )


  (let* ((resultado-flujo (buscar-flujo sistema keyword)))
    (if (null? resultado-flujo)
        '("Opción no encontrada")
        (let ((flujo (cdr resultado-flujo)))
          (if (null? flujo)
              '("Opción no encontrada")
              (let ((numero (string->number keyword)))
                (if (and (number? numero) (>= numero 0))
                    (seleccionar-opcion flujo numero)
                     (list
                      (op 1 2 3 keyword))
                     )))))))

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
  (cond
    ((null? sistema) '()) ; Si no se encuentra en ningún chatbot
    ((string=? (caar sistema) usuario) (cdar sistema)) ; Si encontramos al usuario, devolvemos su flujo
    (else (find-user-flow (cdr sistema) usuario))))

;Función para generar números pseudoaleatorios:
;Funcion para la funcion de simulate (no implementada)
(define (myRandom Xn)
  (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

;----------------------------------------------------------------------------------------------------
; Seccion de Scrips de pruebas
;ACLARACION : Hay algunas funciones que poseen una variacion con respecto al scrips
; deje debajo del scrips la implementacion equivalente...........
;----------------------------------------------------------------------------------------------------
;;Script de Pruebas N°2

;(define op1 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
(define op1 (op 1 1 1 "viajar"))
;(define op2 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define op2 (op 2 2 1 "estudiar"))

;(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op2 op2 op1)) 
(define f10(flow "Flujo principal Chatbot Bienvenido ¿Que te gustaria hacer?" op1 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
;(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada            
(define f11(flow-add-option f10 op1)) ;se intenta añadir opción duplicada
;(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
(define cb0 (chatbot "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10 f10))

;Chatbot1
;(define op3 (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York"))
(define op3 (op 5 1 2 "new york" ))
;(define op4 (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel"))
(define op4 (op 7 1 1 "paris" ))
;(define op5 (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine"))
(define op5 (op 6 1 1 "torres del paine" ))
;(define op6 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver"))
(define op6 (op 8 0 1 "volver" ))

;Opciones segundo flujo Chatbot1
;(define op7 (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park"))
(define op7 (op 20 1 2 "central park" ))
;(define op8 (option 2 "2) Museos" 1 2 "Museo"))
(define op8 (op 10 1 2 "museos" ))
;(define op9 (option 3 "3) Ningún otro atractivo" 1 3 "Museo"))
(define op9 (op 11 1 3 "ningun otro atractivo"))
;(define op10 (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir"))
(define op10 (op 16 1 1 "cambiar destino" ))
;(define op11 (option 1 "1) Solo" 1 3 "Solo"))
(define op11 (op 9 1 3 "solo" ))
;(define op12 (option 2 "2) En pareja" 1 3 "Pareja"))
(define op12 (op 12 1 3 "en pareja" ))
;(define op13 (option 3 "3) En familia" 1 3 "Familia"))
(define op13 (op 13 1 3 "en familia" ))
;(define op14 (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos"))
(define op14 (op 15 1 2 "agregar mas atractivos" ))
;(define op15 (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino"))
(define op15 (op 14 1 1 "en realidad quiero otro destino" ))

;(define f20 (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
(define f20(flow "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6))
;(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
(define f21(flow "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10))
;(define f22 (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
(define f22(flow "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15))
;(define cb1 (chatbot 1 "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f20 f21 f22))
(define cb1 (chatbot "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f22 f21 f20))

;Chatbot2
;(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica"))
(define op16 (op 17 2 1 "carrera tecnica" ))
;(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado"))
(define op17 (op 18 2 1 "postgrado" ))
;(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar"))
(define op18 (op 19 0 1 "volver" ))


;(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
(define f30(flow "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18))
;(define cb2 (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))
(define cb2(chatbot "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30))

;Sistema
;(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2))
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2 ))
;(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
(define s1 (system-add-chatbot s0 cb2 )) 
;(define s2 (system-add-user s1 "user1"))
(define s2 (system-add-user s1 "user1"))
;(define s3 (system-add-user s2 "user2"))
(define s3 (system-add-user s2 "user2"))
;(define s4 (system-add-user s3 "user2"))
(define s4 (system-add-user s3 "user2"))
;(define s5 (system-add-user s4 "user3"))
(define s5 (system-add-user s4 "user3"))
;(define s6 (system-login s5 "user8"))
(define s6 (system-login s5 "user1"))
;(define s7 (system-login s6 "user1"))
(define s7 (system-login s6 "user1"))
;(define s8 (system-login s7 "user2"))
(define s8 (system-login s7 "user2"))
;(define s9 (system-logout s8))
(define s9 (system-logout s8 "user2"))
;(define s10 (system-login s9 "user2"))
(define s10 (system-login s9 "user2"))


(define s11(system-talk-rec s10 "viajar"))
;(define s12(system-talk-rec s11 "1"))
;(define s13 (system-talk-rec s12 "1"))
;(define s14 (system-talk-rec s13 "museo"))
;(define s15 (system-talk-rec s14 "1"))
;(define s16 (system-talk-rec s15 "3"))
;(define s17 (system-talk-rec s16 "5"))
;(display (system-synthesis s17 "user2"))
;(system-simulate s0 5 32131)  ;no implementada


;---------------------------------------------------------------------------------------------------------------------------

;Script de Pruebas N°1
;Para usar este Scrips se debe poner en comentario la seccion de arriba.
;Aclaracion : Cada funcion debajo de esta se encuentra la implementacion (fijarse en como luce el scrips de pruebas n°2)

;----------------------------------------------------------------------------------------------------------------------------

;(define op1 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
;(define op1 (op 1 1 1 "viajar"))
;(define op2 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
;(define op2 (op 2 2 1 "estudiar"))
;(define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
;(define f10(flow "flujo1" op1 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
;(define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
;(define f11(flow-add-option f10 op1))
;(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
;(define cb0 (chatbot "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10 f10))
;(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0))
;(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0))
;(define s1 (system-add-chatbot s0 cb0)) ;igual a s0
;(define s1(system-add-chatbot s0 cb0))
;(define s2 (system-add-user s1 "user1"))
;(define s2 (system-add-user s1 "user1"))
;(define s3 (system-add-user s2 "user2"))
;(define s3 (system-add-user s2 "user2"))
;(define s4 (system-add-user s3 "user2")) ;solo añade un ocurrencia de user2
;(define s4 (system-add-user s3 "user2"))
;(define s5 (system-add-user s4 "user3"))
;(define s5 (system-add-user s4 "user3"))
;(define s6 (system-login s5 "user8")) ;user8 no existe. No inicia sesión
;(define s6 (system-login s5 "user1"))
;(define s7 (system-login s6 "user1"))
;(define s7 (system-login s6 "user1"))
;(define s8 (system-login s7 "user2"))  ;no permite iniciar sesión a user2, pues user1 ya inició sesión
;(define s8 (system-login s7 "user2"))
;(define s9 (system-logout s8))
;(define s9 (system-logout s8 "user2"))
;(define s10 (system-login s9 "user2"))  
;(define s10 (system-login s9 "user2"))

;-------------------------------------------------------------------------------------------------------------------------

;OTROS EJEMPLOS (en caso de que no se vean, ir a los archivos por separado)

;lista aqui ir colocando todas las opciones
(define opciones-disponibles (list op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 op12 op13 op14 op15))

;Ejemplos de uso flow
(define f100(flow "flujo1" op1)) ;creacion de otro flujo
(define f101(flow "flujo1" op2))
(define f102(flow "flujo2" op3))
(define f098(flow "flujo1" op1 op2)) ;primera forma de ver los flujos
(define f0999(flow-aux "flujo1" 2)) ;segunda forma de ver los flujos

;Ejemplos de flow-add-option
(define f112(flow-add-option f100 op1))
(define f123(flow-add-option f112 op2))
(define f124(flow-add-option f123 op3))

;Ejemplo de uso chatbot
(define cb1001(chatbot "Asistente"  "Bienvenido ¿Qué te gustaría hacer?"  1 f112))
(define cb1002(chatbot "Asistente1"  "Bienvenido ¿Que planeas?" 2 f123))
(define cb1003(chatbot "Asistente2"  "Bienvenido ¿A donde iras?"  1 f102))

;Ejemplo de uso chatbot-add-flow
(define cb13 (chatbot-add-flow cb1001 f112))
(define cb14 (chatbot-add-flow cb1002 f101))
(define cb15 (chatbot-add-flow cb1003 f102))

;Ejemplo de uso system
(define s01(system "newSystem1" cb1001))
(define s02(system "newSystem2" cb1002))
(define s03(system "newSystem3" cb1003))

;Ejemplo de uso system-add-chatbot
(define s130 (system-add-chatbot s01 cb1002))
(define s131 (system-add-chatbot s02 cb1003))
(define s132 (system-add-chatbot s03 cb1001))

;Ejemplo de uso system-add-user
(define s22 (system-add-user s03 "user1"))
(define s33 (system-add-user s22 "user2"))
(define s44 (system-add-user s33 "user2")) ;solo añade un ocurrencia de user2
(define s55 (system-add-user s44 "user3"))

;Ejemplo de uso system-login

(define s66 (system-login s55 "user1")) ; Devolverá "Se ha ingresado a sesión correctamente." porque "user0" está en la lista de usuarios.
(define s77 (system-login s55 "user9")) ; Devolverá "No se ha podido ingresar a sesión. El usuario no existe." porque "user9" no está en la lista de usuarios.
(define s88 (system-login s55 42)) ; Devolverá "Error: Argumentos no válidos." porque el usuario no es una cadena.

;Ejemplo de uso system-logout

(define s99 (system-logout s33 "user1"))
(define s100 (system-logout s33 "user2"))
(define s101 (system-logout s33 "user3"))

;Ejemplo de uso system-talk-rec
;(define s102 (system-talk-rec s01 "estudiar"))

;Ejemplo de uso system-talk-norec
;(define s103 (system-talk-norec s02 "viajar"))

; Ejemplo de uso system-synthesis
;(define s1000 (system-add-user s01 "user0"))
;(define s1001 (system-add-chatbot s1000 cb13))
;(define s1002 (system-synthesis s1001 "user0"))




