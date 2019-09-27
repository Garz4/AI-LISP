;;=================================================================================================
;;  tic-tac-toe.lisp
;;  Programa que simula el juego tic-tac-toe en entorno 4x4 y jugando contra una inteligencia artificial.
;;  
;;  Programado por: Uriel García Rivas
;;  Instituto Politécnico Nacional
;;=================================================================================================

;;===================================== CÓDIGO DE TIC TAC TOE =====================================
(defparameter *TABLERO* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(defparameter *JUGADORES* '(X O))
(defparameter *TURNO* 0)
(defun flipBit (bit)
  "<NO DESTRUCTIVA> Cambia de estado al bit que recibe."
  (if (equal bit 0) 1 0)
)
(defun imprimeTablero ()
  "<NO DESTRUCTIVA> Imprime el tablero de manera agradable para la vista."
  (format t "~A | ~A | ~A | ~A~%" (nth 0 *TABLERO*) (nth 1 *TABLERO*) (nth 2 *TABLERO*) (nth 3 *TABLERO*))
  (format t "-----------------~%")
  (format t "~A | ~A | ~A | ~A~%" (nth 4 *TABLERO*) (nth 5 *TABLERO*) (nth 6 *TABLERO*) (nth 7 *TABLERO*))
  (format t "-----------------~%")
  (format t "~A | ~A | ~A | ~A~%" (nth 8 *TABLERO*) (nth 9 *TABLERO*) (nth 10 *TABLERO*) (nth 11 *TABLERO*))
  (format t "-----------------~%")
  (format t "~A | ~A | ~A | ~A~%" (nth 12 *TABLERO*) (nth 13 *TABLERO*) (nth 14 *TABLERO*) (nth 15 *TABLERO*))
)
(defun modifica (pos)
  "<DESTRUCTIVA> Añade al tablero la jugada realizada."
  (if (or (equal (nth pos *TABLERO*) 'X) (equal (nth pos *TABLERO*) 'O)) NIL ;;Primero verifica que no haya una jugada ya colocada antes...
                                                                         (setf (nth pos *TABLERO*) (nth *TURNO* *JUGADORES*) ) ;;Y luego aplica.
  )
)
(defun reinicio ()
  "<DESTRUCTIVA> Reinicia el juego."
  (setf *TABLERO* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
  (setf *TURNO* 0)
)
(defun verificaGane ()
  "<NO DESTRUCTIVA> Verifica si el tablero actual cumple con las condiciones de gane."
  (cond
    ((and (equal (nth 0 *TABLERO*) (nth 1 *TABLERO*)) (equal (nth 0 *TABLERO*) (nth 2 *TABLERO*)) (equal (nth 0 *TABLERO*) (nth 3 *TABLERO*)) ) T)        ;; Verificar
    ((and (equal (nth 4 *TABLERO*) (nth 5 *TABLERO*)) (equal (nth 4 *TABLERO*) (nth 6 *TABLERO*)) (equal (nth 4 *TABLERO*) (nth 7 *TABLERO*)) ) T)        ;; victoria
    ((and (equal (nth 8 *TABLERO*) (nth 9 *TABLERO*)) (equal (nth 8 *TABLERO*) (nth 10 *TABLERO*)) (equal (nth 8 *TABLERO*) (nth 11 *TABLERO*)) ) T)      ;; en líneas
    ((and (equal (nth 12 *TABLERO*) (nth 13 *TABLERO*)) (equal (nth 12 *TABLERO*) (nth 14 *TABLERO*)) (equal (nth 12 *TABLERO*) (nth 15 *TABLERO*)) ) T)  ;; horizontales.
    
    ((and (equal (nth 0 *TABLERO*) (nth 4 *TABLERO*)) (equal (nth 0 *TABLERO*) (nth 8 *TABLERO*)) (equal (nth 0 *TABLERO*) (nth 12 *TABLERO*)) ) T)       ;; Verificiar
    ((and (equal (nth 1 *TABLERO*) (nth 5 *TABLERO*)) (equal (nth 1 *TABLERO*) (nth 9 *TABLERO*)) (equal (nth 1 *TABLERO*) (nth 13 *TABLERO*)) ) T)       ;; victoria
    ((and (equal (nth 2 *TABLERO*) (nth 6 *TABLERO*)) (equal (nth 2 *TABLERO*) (nth 10 *TABLERO*)) (equal (nth 2 *TABLERO*) (nth 14 *TABLERO*)) ) T)      ;; en líneas
    ((and (equal (nth 3 *TABLERO*) (nth 7 *TABLERO*)) (equal (nth 3 *TABLERO*) (nth 11 *TABLERO*)) (equal (nth 3 *TABLERO*) (nth 15 *TABLERO*)) ) T)      ;; verticales.
    
    ((and (equal (nth 0 *TABLERO*) (nth 5 *TABLERO*)) (equal (nth 0 *TABLERO*) (nth 10 *TABLERO*)) (equal (nth 0 *TABLERO*) (nth 15 *TABLERO*)) ) T)       ;; Verificar victoria
    ((and (equal (nth 3 *TABLERO*) (nth 6 *TABLERO*)) (equal (nth 3 *TABLERO*) (nth 9 *TABLERO*)) (equal (nth 3 *TABLERO*) (nth 12 *TABLERO*)) ) T)        ;; en diagonales.
    
    (T NIL)
  )
)

;;=================================================================================================


;;==================================== CÓDIGO DE AGENTE JUGADOR ===================================
(defparameter *OPS* '(:0 :1 :2 :3 :4 :5 :6 :7 :8 :9 :10 :11 :12 :13 :14 :15) )
(defun listaEstado (estado)
  "<NO DESTRUCTIVA> Devuelve una lista exactamente igual a la lista recibida."
  (list (nth 0 estado) (nth 1 estado) (nth 2 estado) (nth 3 estado) (nth 4 estado)
	(nth 5 estado) (nth 6 estado) (nth 7 estado) (nth 8 estado) (nth 9 estado)
	(nth 10 estado) (nth 11 estado) (nth 12 estado) (nth 13 estado) (nth 14 estado) (nth 15 estado)
  )
)
(defun verificaOperador (estado op)
  "<NO DESTRUCTIVA> Verifica que la casilla en donde se intentará colocar la jugada no esté ocupada previamente por otra letra."
  (case op
    (:0 (if (or (equal (nth 0 estado) 'X) (equal (nth 0 estado) 'O)) NIL T))
    (:1 (if (or (equal (nth 1 estado) 'X) (equal (nth 1 estado) 'O)) NIL T))
    (:2 (if (or (equal (nth 2 estado) 'X) (equal (nth 2 estado) 'O)) NIL T))
    (:3 (if (or (equal (nth 3 estado) 'X) (equal (nth 3 estado) 'O)) NIL T))
    (:4 (if (or (equal (nth 4 estado) 'X) (equal (nth 4 estado) 'O)) NIL T))
    (:5 (if (or (equal (nth 5 estado) 'X) (equal (nth 5 estado) 'O)) NIL T))
    (:6 (if (or (equal (nth 6 estado) 'X) (equal (nth 6 estado) 'O)) NIL T))
    (:7 (if (or (equal (nth 7 estado) 'X) (equal (nth 7 estado) 'O)) NIL T))
    (:8 (if (or (equal (nth 8 estado) 'X) (equal (nth 8 estado) 'O)) NIL T))
    (:9 (if (or (equal (nth 9 estado) 'X) (equal (nth 9 estado) 'O)) NIL T))
    (:10 (if (or (equal (nth 10 estado) 'X) (equal (nth 10 estado) 'O)) NIL T))
    (:11 (if (or (equal (nth 11 estado) 'X) (equal (nth 11 estado) 'O)) NIL T))
    (:12 (if (or (equal (nth 12 estado) 'X) (equal (nth 12 estado) 'O)) NIL T))
    (:13 (if (or (equal (nth 13 estado) 'X) (equal (nth 13 estado) 'O)) NIL T))
    (:14 (if (or (equal (nth 14 estado) 'X) (equal (nth 14 estado) 'O)) NIL T))
    (:15 (if (or (equal (nth 15 estado) 'X) (equal (nth 15 estado) 'O)) NIL T))
    (T "Error")
  )
)
(defun finDeJuego? (estado)
  "<NO DESTRUCTIVA> Verifica que cada casilla esté ocupada por alguna letra para que sea fin de juego."
  (if (and
       (or (equal (nth 0 estado) 'X) (equal (nth 0 estado) 'O))
       (or (equal (nth 1 estado) 'X) (equal (nth 1 estado) 'O))
       (or (equal (nth 2 estado) 'X) (equal (nth 2 estado) 'O))
       (or (equal (nth 3 estado) 'X) (equal (nth 3 estado) 'O))
       (or (equal (nth 4 estado) 'X) (equal (nth 4 estado) 'O))
       (or (equal (nth 5 estado) 'X) (equal (nth 5 estado) 'O))
       (or (equal (nth 6 estado) 'X) (equal (nth 6 estado) 'O))
       (or (equal (nth 7 estado) 'X) (equal (nth 7 estado) 'O))
       (or (equal (nth 8 estado) 'X) (equal (nth 8 estado) 'O))
       (or (equal (nth 9 estado) 'X) (equal (nth 9 estado) 'O))
       (or (equal (nth 10 estado) 'X) (equal (nth 10 estado) 'O))
       (or (equal (nth 11 estado) 'X) (equal (nth 11 estado) 'O))
       (or (equal (nth 12 estado) 'X) (equal (nth 12 estado) 'O))
       (or (equal (nth 13 estado) 'X) (equal (nth 13 estado) 'O))
       (or (equal (nth 14 estado) 'X) (equal (nth 14 estado) 'O))
       (or (equal (nth 15 estado) 'X) (equal (nth 15 estado) 'O))
      )
      T NIL
  )
)
(defun aplicaOperador (estado op turnoSimulado)
  "<DESTRUCTIVA> Aplica el operador deseado sobre el estado recibido."
  (let (
	(listaRetorno (listaEstado estado))
       )
    (case op
      (:0 (setf (nth 0 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:1 (setf (nth 1 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:2 (setf (nth 2 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:3 (setf (nth 3 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:4 (setf (nth 4 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:5 (setf (nth 5 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:6 (setf (nth 6 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:7 (setf (nth 7 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:8 (setf (nth 8 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:9 (setf (nth 9 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:10 (setf (nth 10 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:11 (setf (nth 11 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:12 (setf (nth 12 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:13 (setf (nth 13 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:14 (setf (nth 14 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (:15 (setf (nth 15 listaRetorno) (nth turnoSimulado *JUGADORES*)))
      (T "Error")
    )
    (return-from aplicaOperador listaRetorno)
  )
)
(defun evaluar (estado turnoSimulado)
  "<NO DESTRUCTIVA> Evalua, conforme a las opciones ganadoras y perdedoras, el estado recibido."
  (let (
	(currentPlayer (nth turnoSimulado *JUGADORES*))
	(enemyPlayer (nth (if (equal turnoSimulado 0) 1 0) *JUGADORES*))
	(opcGanadoras 0)
	(opcPerdedoras 0)
	)
    ;;=========OPCIONES GANADORAS========
    (if (and (not (equal (nth 0 estado) enemyPlayer)) (not (equal (nth 1 estado) enemyPlayer)) (not (equal (nth 2 estado) enemyPlayer)) (not (equal (nth 3 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 4 estado) enemyPlayer)) (not (equal (nth 5 estado) enemyPlayer)) (not (equal (nth 6 estado) enemyPlayer)) (not (equal (nth 7 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 8 estado) enemyPlayer)) (not (equal (nth 9 estado) enemyPlayer)) (not (equal (nth 10 estado) enemyPlayer)) (not (equal (nth 11 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 12 estado) enemyPlayer)) (not (equal (nth 13 estado) enemyPlayer)) (not (equal (nth 14 estado) enemyPlayer)) (not (equal (nth 15 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    
    (if (and (not (equal (nth 0 estado) enemyPlayer)) (not (equal (nth 4 estado) enemyPlayer)) (not (equal (nth 8 estado) enemyPlayer)) (not (equal (nth 12 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 1 estado) enemyPlayer)) (not (equal (nth 5 estado) enemyPlayer)) (not (equal (nth 9 estado) enemyPlayer)) (not (equal (nth 13 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 2 estado) enemyPlayer)) (not (equal (nth 6 estado) enemyPlayer)) (not (equal (nth 10 estado) enemyPlayer)) (not (equal (nth 14 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 3 estado) enemyPlayer)) (not (equal (nth 7 estado) enemyPlayer)) (not (equal (nth 11 estado) enemyPlayer)) (not (equal (nth 15 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)

    (if (and (not (equal (nth 0 estado) enemyPlayer)) (not (equal (nth 5 estado) enemyPlayer)) (not (equal (nth 10 estado) enemyPlayer)) (not (equal (nth 15 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    (if (and (not (equal (nth 3 estado) enemyPlayer)) (not (equal (nth 6 estado) enemyPlayer)) (not (equal (nth 9 estado) enemyPlayer)) (not (equal (nth 12 estado) enemyPlayer))) (setq opcGanadoras (+ opcGanadoras 1)) NIL)
    ;;===================================
    
    ;;========OPCIONES PERDEDORAS========
    (if (and (not (equal (nth 0 estado) currentPlayer)) (not (equal (nth 1 estado) currentPlayer)) (not (equal (nth 2 estado) currentPlayer)) (not (equal (nth 3 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 4 estado) currentPlayer)) (not (equal (nth 5 estado) currentPlayer)) (not (equal (nth 6 estado) currentPlayer)) (not (equal (nth 7 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 8 estado) currentPlayer)) (not (equal (nth 9 estado) currentPlayer)) (not (equal (nth 10 estado) currentPlayer)) (not (equal (nth 11 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 12 estado) currentPlayer)) (not (equal (nth 13 estado) currentPlayer)) (not (equal (nth 14 estado) currentPlayer)) (not (equal (nth 15 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    
    (if (and (not (equal (nth 0 estado) currentPlayer)) (not (equal (nth 4 estado) currentPlayer)) (not (equal (nth 8 estado) currentPlayer)) (not (equal (nth 12 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 1 estado) currentPlayer)) (not (equal (nth 5 estado) currentPlayer)) (not (equal (nth 9 estado) currentPlayer)) (not (equal (nth 13 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 2 estado) currentPlayer)) (not (equal (nth 6 estado) currentPlayer)) (not (equal (nth 10 estado) currentPlayer)) (not (equal (nth 14 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 3 estado) currentPlayer)) (not (equal (nth 7 estado) currentPlayer)) (not (equal (nth 11 estado) currentPlayer)) (not (equal (nth 15 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)

    (if (and (not (equal (nth 0 estado) currentPlayer)) (not (equal (nth 5 estado) currentPlayer)) (not (equal (nth 10 estado) currentPlayer)) (not (equal (nth 15 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    (if (and (not (equal (nth 3 estado) currentPlayer)) (not (equal (nth 6 estado) currentPlayer)) (not (equal (nth 9 estado) currentPlayer)) (not (equal (nth 12 estado) currentPlayer))) (setq opcPerdedoras (+ opcPerdedoras 1)) NIL)
    ;;===================================
    ;(format t "player:~A, enemy:~A, nth 0 estado:~A <G:~A P:~A>~%" currentPlayer enemyPlayer (nth 0 estado) opcGanadoras opcPerdedoras)
    ;(format t "~A~%~%" estado)
    (return-from evaluar (- opcGanadoras opcPerdedoras))
  )
)
(defun negaMax-alphaBeta (estado profundidad maxProf alfa beta turnoSimulado op)
  "<NO DESTRUCTIVA> Algoritmo Nega Max con podas Alpha Beta."
  (if (or (finDeJuego? estado) (equal profundidad maxProf)) (return-from negaMax-alphaBeta (list (evaluar estado turnoSimulado) op))
      (let (
	    (mejorMov NIL)
	    (mejorValor most-negative-fixnum)
	    (nuevoEstado NIL)
	    (valor NIL)
	   )
	(loop for i in *OPS* do
	     (if (verificaOperador estado i) (progn
					       (setq nuevoEstado (aplicaOperador estado i turnoSimulado))
					       (setq valor (negaMax-alphaBeta nuevoEstado (+ profundidad 1) maxProf (* beta -1) (* (if (> alfa mejorValor) alfa mejorValor) -1 ) (flipBit turnoSimulado) i) )
					       ;(format t "(~A ~A)~%" (first valor) (second valor))
					       (setf (first valor) (* (first valor) -1))
					       (if (> alfa beta) (return-from negaMax-alphaBeta (list (evaluar estado turnoSimulado) op)))
					       (if (> (first valor) mejorValor) (progn
										  (setq mejorValor (first valor))
										  (setq mejorMov i)
										  (if (>= mejorValor beta) (return-from negaMax-alphaBeta (list mejorValor mejorMov)))
					                                        )
					       )
	                                     )
	     )
        )
	(return-from negaMax-alphaBeta (list mejorValor mejorMov))
      )
  )
)
(defun juegaIA (posicion)
  "<DESTRUCTIVA> Función del agente jugador. Modifica el tablero conforme a la posición que considera la mejor."
  (if (null (modifica posicion)) NIL (progn
                                        (if (verificaGane) (format t "¡Perdiste contra la I.A.!~%") NIL)
				        (format t "La I.A. jugó: <~A>~%" posicion)
                                        (imprimeTablero)
                                        (setq *TURNO* (flipBit *TURNO*))
                                     )
  )
)
(defun agenteJugador (estado)
  "<DESTRUCTIVA> Llama al algoritmo Nega Max para decidir sobre la mejor jugada a realizar, y luego la ejecuta."
  (let (
	(jugada (negaMax-alphaBeta estado 0 5  most-negative-fixnum  most-positive-fixnum 1 :0))
	)
    ;(setq jugada (negaMax-alphaBeta estado 0 5  most-negative-fixnum  most-positive-fixnum 1 :0))
    (case (second jugada)
      (:0 (juegaIA 0))
      (:1 (juegaIA 1))
      (:2 (juegaIA 2))
      (:3 (juegaIA 3))
      (:4 (juegaIA 4))
      (:5 (juegaIA 5))
      (:6 (juegaIA 6))
      (:7 (juegaIA 7))
      (:8 (juegaIA 8))
      (:9 (juegaIA 9))
      (:10 (juegaIA 10))
      (:11 (juegaIA 11))
      (:12 (juegaIA 12))
      (:13 (juegaIA 13))
      (:14 (juegaIA 14))
      (:15 (juegaIA 15))
      (T "Error")
    )
  )
)
;;=================================================================================================
(defun juega (posicion)
  "<DESTRUCTIVA> Función del jugador humano. Modifica el tablero conforme a la posición que quiere jugar."
  (if (null (modifica posicion)) NIL (progn
				        (format t "Jugaste: <~A>~%" posicion)
                                        (imprimeTablero)
                                        (setq *TURNO* (flipBit *TURNO*))
					(if (verificaGane) (format t "¡Le ganaste a la I.A.!~%") (agenteJugador (listaEstado *TABLERO*)) )
					
                                     )
  )
)
(defun iniciaJuego ()
  "<DESTRUCTIVA> Reinicia el juego dando indicaciones al jugador humano sobre cómo jugar."
  (reinicio)
  (format t "Bienvenido a Tic-Tac-Toe de 4x4, jugando contra una inteligencia artificial.~%")
  (imprimeTablero)
  (format t "Para jugar, ingrese '(juega <N>)' en la terminal, siendo <N> el número del tablero en el que desea colocar su jugada.~%")
)
"Para iniciar, ingrese '(iniciaJuego)' en la terminal."
