#lang racket
(require "main_19800734_RomeroMartinez.rkt")

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






