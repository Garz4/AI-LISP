;; MIT License
;;
;; Copyright (c) 2021 Uriel Rivas
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; https://github.com/Garz4/artificial-intelligence/blob/master/LICENSE

;--------------------------------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------
;;;------------------------------------------------------------------------------------------------
;;;;   nine-mens-morris.lisp
;;;;
;;;;      Programa que simula el entorno del antigüo juego de estrategia "El molino de los 9".
;;;;      Jugando contra una inteligencia artificial.
;;;;
;;;;      Programado por: Uriel García Rivas
;;;;      Instituto Politécnico Nacional
;;;------------------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------

;;          -- Guía de interpretaciones:
;;               'estado': Un estado es una lista con 24 elementos (números o letras 'A' o 'B') representando el tablero.
;;          -- Guía de funciones importantes:
;;               'evaluar': <NO DESTRUCTIVA> Evalúa el estado recibido.
;;                          Mandada a llamar por:
;;                                'NegaMax-AlphaBeta'
;;                          Devuelve:
;;                                Un número con la función de evaluación: 'OpcionesGanadoras - OpcionesPerdedoras'.
;;
;;               'NegaMax-AlphaBeta': <NO DESTRUCTIVA> Algoritmo NegaMax con podas Alpha-Beta.
;;                          Mandada a llamar por:
;;                                'agenteJugador'
;;                                'NegaMax-AlphaBeta'
;;                          Devuelve:
;;                                Una lista en forma: (<Valor> <Operación>), donde Valor="Valor resultante de la evaluación." y Operación="Operador necesario para llegar a dicha evaluación";
;;                                                                           la Operación puede ser un número (indicando la posición) o una lista conteniendo dos números (posición inicial, posición final).
;;
;;               'agenteJugador': <DESTRUCTIVA> Decide, usando NegaMax, la mejor jugada y la aplica. <--- Es como el "cerebro" de mi agente jugadora.
;;                          Mandada a llamar por:
;;                                'colocar'
;;                                'matar'
;;                                'mover'
;;                                'volar'
;;                          Devuelve:
;;                                Nada en particular.

;;  Disfrute su estadía. ^_^

;;============================================================================================================================================================================================
;;=========================================================================CÓDIGO DEL JUEGO===================================================================================================
;;============================================================================================================================================================================================
(defparameter *TABLERO* '(00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
(defparameter *PIEZASHUMANA* 9) ;;Piezas restantes globales
(defparameter *PIEZASHUMANACOLOCAR* 9) ;; Piezas restantes por colocar
(defparameter *PIEZASIA* 9) ;;Piezas restantes globales
(defparameter *PIEZASIACOLOCAR* 9) ;; Piezas restantes por colocar
(defparameter *JUGADORAS* '(A B))
(defparameter *ETAPA* 1) ;; Número que indica la etapa actual: 1="Etapa de colocación de piezas iniciales.", 2="Etapa de movimiento de piezas.".
(defparameter *VUELOHUMANA* 0) ;; Bit que indica si la jugadora humana se encuentra actualmente en la etapa 3, "Etapa de 'vuelo' de piezas.".
(defparameter *VUELOIA* 0) ;; Bit que indica si el agente jugador se encuentra actualmente en la etapa 3, "Etapa de 'vuelo' de piezas.".
(defparameter *TURNO* 0)
(defparameter *CEMENTERIO* '(- - - - - - - - - - - - - - - - - -))
(defparameter *BANDERAASESINA* 0) ;; Bit que indica si la jugadora en turno tiene la opción o no de matar una pieza enemiga...
(defun flipBit (bit)
  "<NO DESTRUCTIVA> Invierte el bit recibido."
  (if (equal bit 0) 1 0)
)
(defun listaEstado (estado)
  "<NO DESTRUCTIVA> Devuelve una lista exactamente igual a la lista recibida. (para evitar posibles modificaciones a la variable.)"
  (list (nth 0 estado) (nth 1 estado) (nth 2 estado) (nth 3 estado) (nth 4 estado)
	(nth 5 estado) (nth 6 estado) (nth 7 estado) (nth 8 estado) (nth 9 estado)
	(nth 10 estado) (nth 11 estado) (nth 12 estado) (nth 13 estado) (nth 14 estado) (nth 15 estado)
	(nth 16 estado) (nth 17 estado) (nth 18 estado) (nth 19 estado) (nth 20 estado)
	(nth 21 estado) (nth 22 estado) (nth 23 estado)
  )
)
(defun imprimeTablero ()
  "<NO DESTRUCTIVA> Imprime el tablero de forma bonita."
  (format t "~%Tablero guía:~%")
  (format t "00  -  -  01  -  -  02~%")
  (format t "|  03  -  04  -  05  |~%")
  (format t "|  |  06  07  08  |  |~%")
  (format t "09 10 11  ++  12 13 14~%")
  (format t "|  |  15  16  17  |  |~%")
  (format t "|  18  -  19  -  20  |~%")
  (format t "21  -  -  22  -  -  23~%")
)
(defun imprimeJuego (banderaJugada op banderaAsesina op2)
  "<NO DESTRUCTIVA> Imprime el tablero actual y la información del juego en curso."
  (format t "~%==========Etapa del juego: ~A==========~%~%" *ETAPA*)
  (if (equal 0 *ETAPA*) ;; Si es 0, la humana gana.
      (progn
	(format t "¡Felicidades! ¡Le ganaste a mi agente jugadora!~%")
	(format t "~%Eres más inteligente que ella :)~%")
	(format t "----------------~%")
	(format t "Tus piezas: ~A~%" *PIEZASHUMANA*)
	(format t "Las piezas de mi agente jugadora: ~A~%" *PIEZASIA*)
      )
  )
  (if (equal -1 *ETAPA*) ;; Si es -1, la agente jugadora gana.
      (progn
	(format t "¡Oh, no! ¡Perdiste contra mi agente jugadora!~%")
	(format t "~%Eso significa que ella no es tan tonta. <3~%")
	(format t "----------------~%")
	(format t "Tus piezas: ~A~%" *PIEZASHUMANA*)
	(format t "Las piezas de mi agente jugadora: ~A~%" *PIEZASIA*)
      )
  )
  (if (equal 1 *ETAPA*)
      (progn
	(format t "Etapa de colocación de piezas iniciales.~%")
	(format t "~%Piezas por colocar:~%")
	(format t "----------------~%")
	(format t "Jugadora humana: ~A~%" *PIEZASHUMANACOLOCAR*)
	(format t "Agente jugador: ~A~%" *PIEZASIACOLOCAR*)
	(format t "----------------~%")
	(format t "~%Funciones disponibles:~%")
        (format t "----------------~%")
	(format t "'colocar': (colocar <N>), donde <N> es la posición en el tablero en donde se desea colocar una pieza.~%")
      )
  )
  (if (and (equal 1 *VUELOHUMANA*) (not (equal -1 *ETAPA*)))
      (progn
	(format t "Etapa de vuelo de piezas.~%")
	(format t "~%Funciones disponibles:~%")
        (format t "----------------~%")
        (format t "'volar': (volar <N1> <N2>), donde <N1> es la posición inicial en el tablero a la cual se desea mover a la posición <N2>. (<N1> puede 'volar' a cualquier posición no ocupada en el tablero.)~%")
	(format t "'mover': (mover <N1> <N2>), donde <N1> es la posición inicial en el tablero a la cual se desea mover a la posición <N2>. (<N1> solo se puede mover a posiciones adyacentes no ocupadas a ella.)~%")
      )
      (if (equal 2 *ETAPA*)
	  (progn
	    (format t "Etapa de movimiento de piezas.~%")
	    (format t "~%Funciones disponibles:~%")
            (format t "----------------~%")
	    (format t "'mover': (mover <N1> <N2>), donde <N1> es la posición inicial en el tablero a la cual se desea mover a la posición <N2>. (<N1> solo se puede mover a posiciones adyacentes no ocupadas a ella.)~%")
	  )
      )
  )
  (if (equal 1 *BANDERAASESINA*) (format t "'matar': (matar <N>), donde <N> es la posición en el tablero a la cual se desea matar una pieza enemiga, mandándola al cementerio y reduciento el número de piezas enemigas.~%Tienes que matar primero antes de que el juego pueda continuar.~%Piezas que se encuentren formando un 3-en-línea no se pueden matar.~%~%Los números del tablero guía son las posiciones del tablero del juego en curso..~%"))
  (format t "----------------~%")
  (format t "~%=======================================~%")
  (if (or (equal 0 *ETAPA*) (equal -1 *ETAPA*)) T (imprimeTablero))
  (format t "~%Tablero del juego en curso:~%")
  (format t "~A  -  -  ~A  -  -  ~A~%" (if (numberp (nth 0 *TABLERO*)) '* (nth 0 *TABLERO*)) (if (numberp (nth 1 *TABLERO*)) '* (nth 1 *TABLERO*)) (if (numberp (nth 2 *TABLERO*)) '* (nth 2 *TABLERO*)))
  (format t "|  ~A  -  ~A  -  ~A  |~%" (if (numberp (nth 3 *TABLERO*)) '* (nth 3 *TABLERO*)) (if (numberp (nth 4 *TABLERO*)) '* (nth 4 *TABLERO*)) (if (numberp (nth 5 *TABLERO*)) '* (nth 5 *TABLERO*)))
  (format t "|  |  ~A  ~A  ~A  |  |~%" (if (numberp (nth 6 *TABLERO*)) '* (nth 6 *TABLERO*)) (if (numberp (nth 7 *TABLERO*)) '* (nth 7 *TABLERO*)) (if (numberp (nth 8 *TABLERO*)) '* (nth 8 *TABLERO*)))
  (format t "~A  ~A  ~A  +  ~A  ~A  ~A~%" (if (numberp (nth 9 *TABLERO*)) '* (nth 9 *TABLERO*)) (if (numberp (nth 10 *TABLERO*)) '* (nth 10 *TABLERO*)) (if (numberp (nth 11 *TABLERO*)) '* (nth 11 *TABLERO*)) (if (numberp (nth 12 *TABLERO*)) '* (nth 12 *TABLERO*)) (if (numberp (nth 13 *TABLERO*)) '* (nth 13 *TABLERO*)) (if (numberp (nth 14 *TABLERO*)) '* (nth 14 *TABLERO*)))
  (format t "|  |  ~A  ~A  ~A  |  |~%" (if (numberp (nth 15 *TABLERO*)) '* (nth 15 *TABLERO*)) (if (numberp (nth 16 *TABLERO*)) '* (nth 16 *TABLERO*)) (if (numberp (nth 17 *TABLERO*)) '* (nth 17 *TABLERO*)))
  (format t "|  ~A  -  ~A  -  ~A  |~%" (if (numberp (nth 18 *TABLERO*)) '* (nth 18 *TABLERO*)) (if (numberp (nth 19 *TABLERO*)) '* (nth 19 *TABLERO*)) (if (numberp (nth 20 *TABLERO*)) '* (nth 20 *TABLERO*)))
  (format t "~A  -  -  ~A  -  -  ~A~%" (if (numberp (nth 21 *TABLERO*)) '* (nth 21 *TABLERO*)) (if (numberp (nth 22 *TABLERO*)) '* (nth 22 *TABLERO*)) (if (numberp (nth 23 *TABLERO*)) '* (nth 23 *TABLERO*)))
  (case banderaJugada
    (1 (progn
	 (format t "La agente jugadora colocó su pieza en ~A.~%" (second op))
       )
    )
    (2 (progn
	 (format t "La agente jugadora movió su pieza de ~A a ~A.~%" (first (second op)) (second (second op)))
       )
    )
    (3 (progn
	 (format t "La agente jugadora voló su pieza de ~A a ~A.~%" (first (second op)) (second (second op)))
       )
    )
    (T T)
  )
  (if banderaAsesina (format t "La agente jugador mató en ~A.~%" op2))
  (format t "~%Cementerio:~%")
  (format t "----------------~%")
  (format t "~A ~A ~A ~A ~A ~A ~A ~A ~A~%" (nth 0 *CEMENTERIO*) (nth 1 *CEMENTERIO*) (nth 2 *CEMENTERIO*) (nth 3 *CEMENTERIO*) (nth 4 *CEMENTERIO*) (nth 5 *CEMENTERIO*) (nth 6 *CEMENTERIO*) (nth 7 *CEMENTERIO*) (nth 8 *CEMENTERIO*))
  (format t "~A ~A ~A ~A ~A ~A ~A ~A ~A~%" (nth 9 *CEMENTERIO*) (nth 10 *CEMENTERIO*) (nth 11 *CEMENTERIO*) (nth 12 *CEMENTERIO*) (nth 13 *CEMENTERIO*) (nth 14 *CEMENTERIO*) (nth 15 *CEMENTERIO*) (nth 16 *CEMENTERIO*) (nth 17 *CEMENTERIO*))
  (format t "----------------~%")
)
(defun obtenerPosiciones (jugadora estado)
  "<NO DESTRUCTIVA> Devuelve una lista oon las posiciones en el tablero de las piezas de la jugadora recibida. (recibe un 'A o 'B)"
  (let (
	(listaRetorno '())
       )
    (loop for i from 0 to 23 do
	 (if (equal (nth i estado) jugadora) (setq listaRetorno (append listaRetorno (list i)) ) )
    )
    (return-from obtenerPosiciones listaRetorno)
  )
)
(defun obtenerAdyacentes (posicion)
  "<NO DESTRUCTIVA> Dada una posición inicial, devuelve una lista con todas las posiciones adyacentes a ella."
  (case posicion
    (0 (list 1 9))
    (1 (list 0 2 4))
    (2 (list 1 14))
    (3 (list 4 10))
    (4 (list 1 3 5 7))
    (5 (list 4 3))
    (6 (list 7 11))
    (7 (list 4 6 8))
    (8 (list 7 12))
    (9 (list 0 21))
    (10 (list 3 9 11 18))
    (11 (list 6 10 15))
    (12 (list 8 13 17))
    (13 (list 5 12 14 20))
    (14 (list 2 13 23))
    (15 (list 11 16))
    (16 (list 15 17 19))
    (17 (list 12 16))
    (18 (list 10 19))
    (19 (list 16 18 20 22))
    (20 (list 13 19))
    (21 (list 9 22))
    (22 (list 19 21 23))
    (23 (list 14 22))
  )
)
(defun esAdyacente? (posicionIni posicionFin)
  "<NO DESTRUCTIVA> Dada una posición inicial, devuelve T si la posición final es adyacente a ella o NIL si no."
  (case posicionIni
    (0 (if (or (equal 1 posicionFin) (equal 9 posicionFin)) T NIL))
    (1 (if (or (equal 0 posicionFin) (equal 2 posicionFin) (equal 4 posicionFin)) T NIL))
    (2 (if (or (equal 1 posicionFin) (equal 14 posicionFin)) T NIL))
    (3 (if (or (equal 4 posicionFin) (equal 10 posicionFin)) T NIL))
    (4 (if (or (equal 1 posicionFin) (equal 3 posicionFin) (equal 5 posicionFin) (equal 7 posicionFin)) T NIL))
    (5 (if (or (equal 4 posicionFin) (equal 13 posicionFin)) T NIL))
    (6 (if (or (equal 7 posicionFin) (equal 11 posicionFin)) T NIL))
    (7 (if (or (equal 4 posicionFin) (equal 6 posicionFin) (equal 8 posicionFin)) T NIL))
    (8 (if (or (equal 7 posicionFin) (equal 12 posicionFin)) T NIL))
    (9 (if (or (equal 0 posicionFin) (equal 21 posicionFin)) T NIL))
    (10 (if (or (equal 3 posicionFin) (equal 9 posicionFin) (equal 11 posicionFin) (equal 18 posicionFin)) T NIL))
    (11 (if (or (equal 6 posicionFin) (equal 10 posicionFin) (equal 15 posicionFin)) T NIL))
    (12 (if (or (equal 8 posicionFin) (equal 13 posicionFin) (equal 17 posicionFin)) T NIL))
    (13 (if (or (equal 5 posicionFin) (equal 12 posicionFin) (equal 14 posicionFin) (equal 20 posicionFin)) T NIL))
    (14 (if (or (equal 2 posicionFin) (equal 13 posicionFin) (equal 23 posicionFin)) T NIL))
    (15 (if (or (equal 11 posicionFin) (equal 16 posicionFin)) T NIL))
    (16 (if (or (equal 15 posicionFin) (equal 17 posicionFin) (equal 19 posicionFin)) T NIL))
    (17 (if (or (equal 12 posicionFin) (equal 16 posicionFin)) T NIL))
    (18 (if (or (equal 10 posicionFin) (equal 19 posicionFin)) T NIL))
    (19 (if (or (equal 16 posicionFin) (equal 18 posicionFin) (equal 20 posicionFin) (equal 22 posicionFin)) T NIL))
    (20 (if (or (equal 13 posicionFin) (equal 19 posicionFin)) T NIL))
    (21 (if (or (equal 9 posicionFin) (equal 22 posicionFin)) T NIL))
    (22 (if (or (equal 19 posicionFin) (equal 21 posicionFin) (equal 23 posicionFin)) T NIL))
    (23 (if (or (equal 14 posicionFin) (equal 22 posicionFin)) T NIL))
  )
)
(defun sePuedeMover? (posicion estado)
  "<NO DESTRUCTIVA> Dada una posición, devuelve T si dicha posición se puede mover a alguna parte adyacente o NIL si no."
  (case posicion
    (0 (if (or (not (or (equal 'A (nth 1 estado)) (equal 'B (nth 1 estado)) )) (not (or (equal 'A (nth 9 estado)) (equal 'B (nth 9 estado)) ))) T NIL))
    (1 (if (or (not (or (equal 'A (nth 0 estado)) (equal '0 (nth 1 estado)) )) (not (or (equal 'A (nth 2 estado)) (equal 'B (nth 2 estado)) )) (not (or (equal 'A (nth 4 estado)) (equal 'B (nth 4 estado)) ))) T NIL))
    (2 (if (or (not (or (equal 'A (nth 1 estado)) (equal 'B (nth 1 estado)) )) (not (or (equal 'A (nth 14 estado)) (equal 'B (nth 14 estado)) ))) T NIL))
    (3 (if (or (not (or (equal 'A (nth 4 estado)) (equal 'B (nth 4 estado)) )) (not (or (equal 'A (nth 10 estado)) (equal 'B (nth 10 estado)) ))) T NIL))
    (4 (if (or (not (or (equal 'A (nth 1 estado)) (equal 'B (nth 1 estado)) )) (not (or (equal 'A (nth 3 estado)) (equal 'B (nth 3 estado)) )) (not (or (equal 'A (nth 5 estado)) (equal 'B (nth 5 estado)) )) (not (or (equal 'A (nth 7 estado)) (equal 'B (nth 7 estado)) ))) T NIL))
    (5 (if (or (not (or (equal 'A (nth 4 estado)) (equal 'B (nth 4 estado)) )) (not (or (equal 'A (nth 13 estado)) (equal 'B (nth 13 estado)) ))) T NIL))
    (6 (if (or (not (or (equal 'A (nth 7 estado)) (equal 'B (nth 7 estado)) )) (not (or (equal 'A (nth 11 estado)) (equal 'B (nth 11 estado)) ))) T NIL))
    (7 (if (or (not (or (equal 'A (nth 4 estado)) (equal 'B (nth 4 estado)) )) (not (or (equal 'A (nth 6 estado)) (equal 'B (nth 6 estado)) )) (not (or (equal 'A (nth 8 estado)) (equal 'B (nth 8 estado)) ))) T NIL))
    (8 (if (or (not (or (equal 'A (nth 7 estado)) (equal 'B (nth 7 estado)) )) (not (or (equal 'A (nth 12 estado)) (equal 'B (nth 12 estado)) ))) T NIL))
    (9 (if (or (not (or (equal 'A (nth 0 estado)) (equal 'B (nth 0 estado)) )) (not (or (equal 'A (nth 21 estado)) (equal 'B (nth 21 estado)) ))) T NIL))
    (10 (if (or (not (or (equal 'A (nth 3 estado)) (equal 'B (nth 3 estado)) )) (not (or (equal 'A (nth 9 estado)) (equal 'B (nth 9 estado)) )) (not (or (equal 'A (nth 11 estado)) (equal 'B (nth 11 estado)) )) (not (or (equal 'A (nth 18 estado)) (equal 'B (nth 18 estado)) ))) T NIL))
    (11 (if (or (not (or (equal 'A (nth 6 estado)) (equal 'B (nth 6 estado)) )) (not (or (equal 'A (nth 10 estado)) (equal 'B (nth 10 estado)) )) (not (or (equal 'A (nth 15 estado)) (equal 'B (nth 15 estado)) ))) T NIL))
    (12 (if (or (not (or (equal 'A (nth 8 estado)) (equal 'B (nth 8 estado)) )) (not (or (equal 'A (nth 13 estado)) (equal 'B (nth 13 estado)) )) (not (or (equal 'A (nth 17 estado)) (equal 'B (nth 17 estado)) ))) T NIL))
    (13 (if (or (not (or (equal 'A (nth 5 estado)) (equal 'B (nth 5 estado)) )) (not (or (equal 'A (nth 12 estado)) (equal 'B (nth 12 estado)) )) (not (or (equal 'A (nth 14 estado)) (equal 'B (nth 14 estado)) )) (not (or (equal 'A (nth 20 estado)) (equal 'B (nth 20 estado)) ))) T NIL))
    (14 (if (or (not (or (equal 'A (nth 2 estado)) (equal 'B (nth 2 estado)) )) (not (or (equal 'A (nth 13 estado)) (equal 'B (nth 13 estado)) )) (not (or (equal 'A (nth 23 estado)) (equal 'B (nth 23 estado)) ))) T NIL))
    (15 (if (or (not (or (equal 'A (nth 11 estado)) (equal 'B (nth 11 estado)) )) (not (or (equal 'A (nth 16 estado)) (equal 'B (nth 16 estado)) ))) T NIL))
    (16 (if (or (not (or (equal 'A (nth 15 estado)) (equal 'B (nth 15 estado)) )) (not (or (equal 'A (nth 17 estado)) (equal 'B (nth 17 estado)) )) (not (or (equal 'A (nth 19 estado)) (equal 'B (nth 19 estado)) ))) T NIL))
    (17 (if (or (not (or (equal 'A (nth 12 estado)) (equal 'B (nth 12 estado)) )) (not (or (equal 'A (nth 16 estado)) (equal 'B (nth 16 estado)) ))) T NIL))
    (18 (if (or (not (or (equal 'A (nth 10 estado)) (equal 'B (nth 10 estado)) )) (not (or (equal 'A (nth 19 estado)) (equal 'B (nth 19 estado)) ))) T NIL))
    (19 (if (or (not (or (equal 'A (nth 16 estado)) (equal 'B (nth 16 estado)) )) (not (or (equal 'A (nth 18 estado)) (equal 'B (nth 18 estado)) )) (not (or (equal 'A (nth 20 estado)) (equal 'B (nth 20 estado)) )) (not (or (equal 'A (nth 22 estado)) (equal 'B (nth 22 estado)) ))) T NIL))
    (20 (if (or (not (or (equal 'A (nth 13 estado)) (equal 'B (nth 13 estado)) )) (not (or (equal 'A (nth 19 estado)) (equal 'B (nth 19 estado)) ))) T NIL))
    (21 (if (or (not (or (equal 'A (nth 9 estado)) (equal 'B (nth 9 estado)) )) (not (or (equal 'A (nth 22 estado)) (equal 'B (nth 22 estado)) ))) T NIL))
    (22 (if (or (not (or (equal 'A (nth 19 estado)) (equal 'B (nth 19 estado)) )) (not (or (equal 'A (nth 21 estado)) (equal 'B (nth 21 estado)) )) (not (or (equal 'A (nth 23 estado)) (equal 'B (nth 23 estado)) ))) T NIL))
    (23 (if (or (not (or (equal 'A (nth 14 estado)) (equal 'B (nth 14 estado)) )) (not (or (equal 'A (nth 22 estado)) (equal 'B (nth 22 estado)) ))) T NIL))
  )
)
(defun es3EnLinea? (posicion estado)
  "<NO DESTRUCTIVA> Dada una posición, verifica si esta tiene un 3 en línea en alguna de sus variantes (horizontal o vertical)."
  (case posicion
    (0 (if (or (and (equal (nth 0 estado) (nth 1 estado)) (equal (nth 0 estado) (nth 2 estado))) (and (equal (nth 0 estado) (nth 9 estado)) (equal (nth 0 estado) (nth 21 estado))) ) T NIL))
    (1 (if (or (and (equal (nth 1 estado) (nth 0 estado)) (equal (nth 1 estado) (nth 2 estado))) (and (equal (nth 1 estado) (nth 4 estado)) (equal (nth 1 estado) (nth 7 estado))) ) T NIL))
    (2 (if (or (and (equal (nth 2 estado) (nth 0 estado)) (equal (nth 2 estado) (nth 1 estado))) (and (equal (nth 2 estado) (nth 14 estado)) (equal (nth 2 estado) (nth 23 estado))) ) T NIL))
    (3 (if (or (and (equal (nth 3 estado) (nth 4 estado)) (equal (nth 3 estado) (nth 5 estado))) (and (equal (nth 3 estado) (nth 10 estado)) (equal (nth 3 estado) (nth 18 estado))) ) T NIL))
    (4 (if (or (and (equal (nth 4 estado) (nth 3 estado)) (equal (nth 4 estado) (nth 5 estado))) (and (equal (nth 4 estado) (nth 1 estado)) (equal (nth 4 estado) (nth 7 estado))) ) T NIL))
    (5 (if (or (and (equal (nth 5 estado) (nth 3 estado)) (equal (nth 5 estado) (nth 4 estado))) (and (equal (nth 5 estado) (nth 13 estado)) (equal (nth 5 estado) (nth 20 estado))) ) T NIL))
    (6 (if (or (and (equal (nth 6 estado) (nth 7 estado)) (equal (nth 6 estado) (nth 8 estado))) (and (equal (nth 6 estado) (nth 11 estado)) (equal (nth 6 estado) (nth 15 estado))) ) T NIL))
    (7 (if (or (and (equal (nth 7 estado) (nth 6 estado)) (equal (nth 7 estado) (nth 8 estado))) (and (equal (nth 7 estado) (nth 1 estado)) (equal (nth 7 estado) (nth 4 estado))) ) T NIL))
    (8 (if (or (and (equal (nth 8 estado) (nth 6 estado)) (equal (nth 8 estado) (nth 7 estado))) (and (equal (nth 8 estado) (nth 12 estado)) (equal (nth 8 estado) (nth 17 estado))) ) T NIL))
    (9 (if (or (and (equal (nth 9 estado) (nth 10 estado)) (equal (nth 9 estado) (nth 11 estado))) (and (equal (nth 9 estado) (nth 0 estado)) (equal (nth 9 estado) (nth 21 estado))) ) T NIL))
    (10 (if (or (and (equal (nth 10 estado) (nth 9 estado)) (equal (nth 10 estado) (nth 11 estado))) (and (equal (nth 10 estado) (nth 3 estado)) (equal (nth 10 estado) (nth 18 estado))) ) T NIL))
    (11 (if (or (and (equal (nth 11 estado) (nth 9 estado)) (equal (nth 11 estado) (nth 10 estado))) (and (equal (nth 11 estado) (nth 6 estado)) (equal (nth 11 estado) (nth 15 estado))) ) T NIL))
    (12 (if (or (and (equal (nth 12 estado) (nth 13 estado)) (equal (nth 12 estado) (nth 14 estado))) (and (equal (nth 12 estado) (nth 8 estado)) (equal (nth 12 estado) (nth 17 estado))) ) T NIL))
    (13 (if (or (and (equal (nth 13 estado) (nth 12 estado)) (equal (nth 13 estado) (nth 14 estado))) (and (equal (nth 13 estado) (nth 5 estado)) (equal (nth 13 estado) (nth 20 estado))) ) T NIL))
    (14 (if (or (and (equal (nth 14 estado) (nth 12 estado)) (equal (nth 14 estado) (nth 13 estado))) (and (equal (nth 14 estado) (nth 2 estado)) (equal (nth 14 estado) (nth 23 estado))) ) T NIL))
    (15 (if (or (and (equal (nth 15 estado) (nth 16 estado)) (equal (nth 15 estado) (nth 17 estado))) (and (equal (nth 15 estado) (nth 6 estado)) (equal (nth 15 estado) (nth 11 estado))) ) T NIL))
    (16 (if (or (and (equal (nth 16 estado) (nth 15 estado)) (equal (nth 16 estado) (nth 17 estado))) (and (equal (nth 16 estado) (nth 19 estado)) (equal (nth 16 estado) (nth 22 estado))) ) T NIL))
    (17 (if (or (and (equal (nth 17 estado) (nth 15 estado)) (equal (nth 17 estado) (nth 16 estado))) (and (equal (nth 17 estado) (nth 8 estado)) (equal (nth 17 estado) (nth 12 estado))) ) T NIL))
    (18 (if (or (and (equal (nth 18 estado) (nth 19 estado)) (equal (nth 18 estado) (nth 20 estado))) (and (equal (nth 18 estado) (nth 3 estado)) (equal (nth 18 estado) (nth 10 estado))) ) T NIL))
    (19 (if (or (and (equal (nth 19 estado) (nth 18 estado)) (equal (nth 19 estado) (nth 20 estado))) (and (equal (nth 19 estado) (nth 16 estado)) (equal (nth 19 estado) (nth 22 estado))) ) T NIL))
    (20 (if (or (and (equal (nth 20 estado) (nth 18 estado)) (equal (nth 20 estado) (nth 19 estado))) (and (equal (nth 20 estado) (nth 5 estado)) (equal (nth 20 estado) (nth 13 estado))) ) T NIL))
    (21 (if (or (and (equal (nth 21 estado) (nth 22 estado)) (equal (nth 21 estado) (nth 23 estado))) (and (equal (nth 21 estado) (nth 0 estado)) (equal (nth 21 estado) (nth 9 estado))) ) T NIL))
    (22 (if (or (and (equal (nth 22 estado) (nth 21 estado)) (equal (nth 22 estado) (nth 23 estado))) (and (equal (nth 22 estado) (nth 16 estado)) (equal (nth 22 estado) (nth 19 estado))) ) T NIL))
    (23 (if (or (and (equal (nth 23 estado) (nth 21 estado)) (equal (nth 23 estado) (nth 22 estado))) (and (equal (nth 23 estado) (nth 2 estado)) (equal (nth 23 estado) (nth 14 estado))) ) T NIL))
  )
)

(defun gana? (estado piezasEnemiga turnoSimulado)
  "<NO DESTRUCTIVA> Verifica si el estado recibido es posición de gane o no para la jugadora recibida."
  (if (> 3 piezasEnemiga) (return-from gana? T) NIL)
  (let (
	(piezasJugadora (obtenerPosiciones (nth turnoSimulado *JUGADORAS*) estado)) ;;Se obtienen las posiciones de las piezas de la agente jugadora.
       )
    (loop for i in piezasJugadora do
	 (if (sePuedeMover? i estado) (return-from gana? NIL) T) ;;Se checa si cada una de ellas se puede mover adyacentemente.
    )
  )
)


;;============================================================================================================================================================================================
;;============================================================================================================================================================================================




;;============================================================================================================================================================================================
;;====================================================================CÓDIGO DE LA AGENTE JUGADOR=============================================================================================
;;============================================================================================================================================================================================
(defparameter *JUGADADEMATANZA* '(0 () ()))
(defparameter *OPSCOLOCAR* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
(defparameter *OPSMATAR* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
(defparameter *OPSMOVER* '(
			   (0 1) (0 9)
			   (1 0) (1 2) (1 4)
			   (2 1) (2 14)
			   (3 4) (3 10)
			   (4 1) (4 3) (4 5) (4 7)
			   (5 4) (5 13)
			   (6 7) (6 11)
			   (7 4) (7 6) (7 8)
			   (8 7) (8 12)
			   (9 1) (9 10) (9 21) 
			   (10 3) (10 9) (10 11) (10 18)
			   (11 6) (11 10) (11 15)
			   (12 8) (12 13) (12 17)
			   (13 5) (13 12) (13 14) (13 20)
			   (14 2) (14 13) (14 23)
			   (15 11) (15 16)
			   (16 15) (16 17) (16 19)
			   (17 12) (17 16)
			   (18 10) (18 19)
			   (19 16) (19 18) (19 20) (19 22)
			   (20 13) (20 19)
			   (21 9) (21 22)
			   (22 19) (22 21) (22 23)
			   (23 14) (23 22)
			  )
)
(defparameter *OPSVOLAR* '())
(loop for i from 0 to 23 do
     (loop for j from 0 to 23 do
	  (if (not (equal i j))
	      (push (list i j) *OPSVOLAR*)
	  )
     )
)
(setq *OPSVOLAR* (reverse *OPSVOLAR*))
(defparameter *HEURÍSTICASAUX* '(NIL NIL NIL)) ;; Lista que indica la activación de las heurísticas auxiliares.
(defparameter *TURNOSIMULADO* 1)
(defparameter *MEMORIAASESINA* '()) ;;Almacena nodos de la forma (<Estado> <Operación>)
(defun validarOperador (estado op turnoSimulado)
  "<NO DESTRUCTIVA> Retorna un valor de verdad indicando si el operador recibido se puede aplicar al estado recibido."
  (if (equal 1 *BANDERAASESINA*)
      (if (and (equal (nth op estado) (nth (flipBit turnoSimulado) *JUGADORAS*))
	       (not (es3EnLinea? op estado))
	  )
	  (return-from validarOperador T)
	  (return-from validarOperador NIL)
      )
      (if (equal *ETAPA* 1)
	  (if (numberp (nth op estado))
	      (return-from validarOperador T)
	      (return-from validarOperador NIL)
	  )
	  (if (equal *VUELOIA* 1)
	      (if (and (numberp (nth (second op) estado))
		       (equal (nth (first op) estado) (nth turnoSimulado *JUGADORAS*))
		  )
		  (return-from validarOperador T)
		  (return-from validarOperador NIL)
	      )
	      (if (and (numberp (nth (second op) estado))
		       (equal (nth (first op) estado) (nth turnoSimulado *JUGADORAS*))
		       (esAdyacente? (first op) (second op))
		  )
		  (return-from validarOperador T)
		  (return-from validarOperador NIL)
	      )
	  )
      )
  )
)
(defun aplicaOperador (estado op turnoSimulado)
  "<NO DESTRUCTIVA> Aplica el operador op al estado recibido y lo devuelve."
  (let (
	(listaRetorno (listaEstado estado))
       )
    (if (equal 1 *BANDERAASESINA*)
	(if (and (equal (nth op estado) (nth (flipBit turnoSimulado) *JUGADORAS*))
		 (not (es3EnLinea? op estado))
	    )
	    (setf (nth op listaRetorno) op)
	)
	(if (equal *ETAPA* 1)
	    (if (numberp (nth op estado))
	        (setf (nth op listaRetorno) (nth turnoSimulado *JUGADORAS*))
	    )
	    (if (equal *VUELOIA* 1)
		(if (and (numberp (nth (second op) estado))
			 (equal (nth (first op) estado) (nth turnoSimulado *JUGADORAS*))
		    )
		    (setf (nth (first op) listaRetorno) (first op))
		    (setf (nth (second op) listaRetorno) (nth turnoSimulado *JUGADORAS*))
	        )
		(if (and (numberp (nth (second op) estado))
			 (equal (nth (first op) estado) (nth turnoSimulado *JUGADORAS*))
			 (esAdyacente? (first op) (second op))
		    )
		    (setf (nth (first op) listaRetorno) (first op))
		    (setf (nth (second op) listaRetorno) (nth turnoSimulado *JUGADORAS*))
		)
	    )
	)
    )
    (return-from aplicaOperador listaRetorno) 
  )
)
(defun hayAlguna? (estado jugadora pos1 pos2 pos3)
  "<NO DESTRUCTIVA> Dadas tres posiciones, devuelve T si en el estado recibido está por lo menos una pieza, o NIL si no hay ninguna."
  (if (or (equal (nth pos1 estado) (nth jugadora *JUGADORAS*))
	  (equal (nth pos2 estado) (nth jugadora *JUGADORAS*))
	  (equal (nth pos3 estado) (nth jugadora *JUGADORAS*))
      )
      T
      NIL
  )
)
(defun heurísticaAsesina (estado)
  "<NO DESTRUCTIVA> Si un estado junto con su operación forma parte de la memoria asesina, lo poda inmediatamente."
  (if (not (null *MEMORIAASESINA*))
      (loop for i in *MEMORIAASESINA* do
	   (if (equal estado i) (return-from heurísticaAsesina T))
      )
  )
  (return-from heurísticaAsesina NIL)
)
(defun evaluar (estado turnoSimulado)
  "<NO DESTRUCTIVA> Evalúa el estado recibido."
  (let (
	(jugadora (obtenerPosiciones (nth turnoSimulado *JUGADORAS*) estado))            ;;Obtenemos las posiciones
	(enemiga (obtenerPosiciones (nth (flipBit turnoSimulado) *JUGADORAS*) estado))   ;;de las jugadoras
	(opcionesGanadoras 0)
	(opcionesPerdedoras 0)
       )
    (loop for i in jugadora do
	 (if (es3EnLinea? i estado) (return-from evaluar most-positive-fixnum)) ;;Si es 3 en línea alguna posición aliada del estado recibido, devuelve +INFINITY.
    )
    (loop for i in enemiga do
	 (if (es3EnLinea? i estado) (if (and (first *HEURÍSTICASAUX*) (not (heurísticaAsesina estado))) ;;Si es 3 en línea alguna posición enemiga del estado recibido, y si está activada la heurística asesina...
					(progn
					  (push estado *MEMORIAASESINA*) ;;guarda el estado junto con la operación...
					  (return-from evaluar most-negative-fixnum) ;;y devuelve -INFINITY.
				        )
					(return-from evaluar most-negative-fixnum) ;;Si no, simplemente devuelve -INFINITY.
				     )
	 )
    )
    ;;-----------------------------------------------------------------------------------------------------
    ;;--Líneas horizontales--------------------------------------------------------------------------------
    ;;-----------------------------------------------------------------------------------------------------
    (if (hayAlguna? estado (flipBit turnoSimulado) 0 1 2) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 0 1 2)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 3 4 5) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 3 4 5)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 6 7 8) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 6 7 8)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 9 10 11) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 9 10 11)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 12 13 14) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 12 13 14)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 15 16 17) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 15 16 17)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 18 19 20) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 18 19 20)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 21 22 23) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 21 22 23)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    ;;-----------------------------------------------------------------------------------------------------

    ;;-----------------------------------------------------------------------------------------------------
    ;;--Líneas verticales----------------------------------------------------------------------------------
    ;;-----------------------------------------------------------------------------------------------------
    (if (hayAlguna? estado (flipBit turnoSimulado) 0 9 21) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 0 9 21)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 3 10 18) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 3 10 18)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 6 11 15) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 6 11 15)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 1 4 7) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 1 4 7)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 16 19 22) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 16 19 22)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 8 12 17) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 8 12 17)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 5 13 20) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 5 13 20)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    (if (hayAlguna? estado (flipBit turnoSimulado) 2 14 23) (setq opcionesPerdedoras (+ opcionesPerdedoras 1)) (if (hayAlguna? estado turnoSimulado 2 14 23)
														 (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 (progn
														   (setq opcionesPerdedoras (+ opcionesPerdedoras 1))
													           (setq opcionesGanadoras (+ opcionesGanadoras 1))
														 )
													      )
    )
    ;;-----------------------------------------------------------------------------------------------------
    (return-from evaluar (- opcionesGanadoras opcionesPerdedoras))
  )
)
(defun heurísticaDeMatanzaInstantánea ()
  "<NO DESTRUCTIVA> Devuelve las operaciones necesarias para 'deshacer' y 'rehacer' un 3-en-línea de la agente jugadora para matar instantáneamente."
  (let (
	(jugadora (obtenerPosiciones 'B *TABLERO*))
	(adyacentes NIL)
       )
    (loop for i in jugadora do
	 (if (es3EnLinea? i *TABLERO*) (progn
					 (setq adyacentes (obtenerAdyacentes i))
					 (loop for j in adyacentes do
					      (if (numberp (nth j *TABLERO*)) (progn
										(setq *JUGADADEMATANZA* (list 1 (list i j) (list j i)))
										(return-from heurísticaDeMatanzaInstantánea T)
									      )
					      )
					 )
				       )

	 )
    )
  )
)
(defun NegaMax-AlphaBeta (estado profundidad maxProf alfa beta turnoSimulado op ops salto)
  "<NO DESTRUCTIVA> Algoritmo NegaMax con podas Alpha-Beta y heurísticas auxiliares (en caso de ser activadas)."
  (if (first *HEURÍSTICASAUX*) (if (heurísticaAsesina estado) (return-from NegaMax-AlphaBeta (list most-negative-fixnum op))) ) ;;Antes que nada, checamos con...
  (if (and (not salto) (second *HEURÍSTICASAUX*)) (if (equal (first (NegaMax-AlphaBeta estado 0 1 alfa beta (flipBit turnoSimulado) op ops 1)) most-negative-fixnum) (return-from NegaMax-AlphaBeta (list most-negative-fixnum op))) ) ;;...las heurísticas auxiliares.
  (if (> alfa beta) (return-from negaMax-alphaBeta (list most-negative-fixnum op))) ;;Si no, checamos la poda alfa beta.
  (if (= turnoSimulado 0)
      (if (gana? estado *PIEZASIA* 1) (return-from NegaMax-AlphaBeta (list (evaluar estado turnoSimulado) op))) ;;O checamos si es estado
      (if (gana? estado *PIEZASHUMANA* 0) (return-from NegaMax-AlphaBeta (list (evaluar estado turnoSimulado) op))) ;;de gane.
  )
  (if (equal profundidad maxProf) (return-from NegaMax-alphaBeta (list (evaluar estado turnoSimulado) op)) ;;O si ya se alcanzó la profundidad máxima.
    (let ( ;;Si no se cumple absolutamente nada de lo de arriba, empezamos NegaMax.
	  (mejorMov NIL)
	  (mejorValor most-negative-fixnum)
	  (nuevoEstado NIL)
	  (valor NIL)
	 )
      (loop for i in ops do
	   (if (validarOperador estado i turnoSimulado) (progn
					     (setq nuevoEstado (aplicaOperador estado i turnoSimulado))
					     (setq valor (NegaMax-AlphaBeta nuevoEstado (+ profundidad 1) maxProf (* beta -1) (* (if (> alfa mejorValor) alfa mejorValor) -1 ) (flipBit turnoSimulado) i ops 0) )
					     ;(format t "(~A ~A)~%" (first valor) (second valor))
					     (setf (first valor) (* (first valor) -1))
					     (if (> (first valor) mejorValor) (progn
										(setq mejorValor (first valor))
										(setq mejorMov i)
										(if (>= mejorValor beta) (return-from NegaMax-AlphaBeta (list mejorValor mejorMov)))
					                                      )
					     )
	                                   )
	   )
      )
    (return-from negaMax-alphaBeta (list mejorValor mejorMov))
    )
  )
)
(defun habilitar (asesina salto matanza)
  "<DESTRUCTIVA> Modifica los valores de la lista *HEURÍSTICASAUX* para indicar cuáles están activadas y cuáles no."
  (setq *HEURÍSTICASAUX* (list asesina salto matanza))
  (format t "Heurística asesina: ~A~%" (if asesina "ACTIVADA" "NO ACTIVADA"))
  (format t "Heurística de salto: ~A~%" (if salto "ACTIVADA" "NO ACTIVADA"))
  (format t "Heurística de matanza instantánea: ~A~%" (if matanza "ACTIVADA" "NO ACTIVADA"))
  (format t "Recuerda: Tú eres la jugadora inicial.~%~%")
  (imprimeJuego -1 -1 NIL -1)
)
(defun colocarIA (posicion)
  "<DESTRUCTIVA> Coloca la pieza del jugador en turno en la posición recibida si no está ocupada."
  (if (and (equal 1 *ETAPA*) ;;Si es la primera etapa, y...
	   (not (equal 'A (nth posicion *TABLERO*))) ;;No hay ninguna pieza
	   (not (equal 'B (nth posicion *TABLERO*))) ;;colocada en ese lugar ya, y...
	   (or (> *PIEZASIACOLOCAR* 0)     ;;Hay suficientes piezas
	       (> *PIEZASHUMANACOLOCAR* 0) ;;para colocar, y...
	   )
	   (equal 0 *BANDERAASESINA*) ;;La bandera asesina no está activada...
      )           ;;Entonces...
      (progn
	(setf (nth posicion *TABLERO*) (nth *TURNO* *JUGADORAS*))
	(setq *PIEZASIACOLOCAR* (- *PIEZASIACOLOCAR* 1))
	(if (equal 0 *PIEZASIACOLOCAR*) (setq *ETAPA* 2))
	(if (gana? *TABLERO* *PIEZASHUMANA* 0)
	    (setq *ETAPA* -1)
	    (if (es3EnLinea? posicion *TABLERO*)
		(setq *BANDERAASESINA* 1)
		(setq *TURNO* (flipBit *TURNO*))
	    )
        )
      )
      NIL
  )
)
(defun matarIA (posicion)
  "<DESTRUCTIVA> Elimina la pieza del enemigo en turno en la posición recibida, reduciendo el número de piezas enemigas en su contador."
  (if (and (equal 1 *BANDERAASESINA*) ;;Si la bandera asesina está activada, y...
           (equal (nth posicion *TABLERO*) (nth (flipBit *TURNO*) *JUGADORAS*) ) ;;En la posición dada está una pieza enemiga, y...
	   (not (es3EnLinea? posicion *TABLERO*)) ;;Dicha posición no está en un 3 en línea...
      )                    ;;Entonces...
      (progn
	(setq *CEMENTERIO* (reverse *CEMENTERIO*))
	(pop *CEMENTERIO*)
	(setq *CEMENTERIO* (reverse *CEMENTERIO*))
	(setq *PIEZASHUMANA* (- *PIEZASHUMANA* 1))
	(if (= 3 *PIEZASHUMANA*) (setq *VUELOHUMANA* 1)) ;;Si le quedan exactamente 3 piezas restantes a la jugadora humana, esta entra en la etapa 3.
	(push 'A *CEMENTERIO*)
	(setq *BANDERAASESINA* 0)
	(setf (nth posicion *TABLERO*) posicion)
	(setq *TURNO* (flipBit *TURNO*))
	(format t "Éxito en el asesinato.~%")
	(if (gana? *TABLERO* *PIEZASHUMANA* 0)
	    (setq *ETAPA* -1)
	)
      )
      NIL
  )
)
(defun moverIA (posicionIni posicionFin)
  "<DESTRUCTIVA> Mueve la pieza de la posicionIni a la posicionFin."
  (if (and (equal 2 *ETAPA*) ;;Si es la segunda etapa, y...
	   (equal (nth posicionIni *TABLERO*) (nth *TURNO* *JUGADORAS*)) ;;En la posición inicial está una pieza de la jugadora en turno, y...
	   (not (or (equal 'A (nth posicionFin *TABLERO*)) (equal 'B (nth posicionFin *TABLERO*)) )) ;;La posición final está libre de piezas de jugadoras, y...
	   (esAdyacente? posicionIni posicionFin) ;;La posición final es adyacente a la posición inicial, y...
	   (equal 0 *BANDERAASESINA*) ;;La bandera asesina no está activada...
      )             ;;Entonces...
      (progn
	(setf (nth posicionIni *TABLERO*) posicionIni)
	(setf (nth posicionFin *TABLERO*) (nth *TURNO* *JUGADORAS*))
	(if (gana? *TABLERO* *PIEZASHUMANA* 0)
	    (setq *ETAPA* -1)
	    (if (es3EnLinea? posicionFin *TABLERO*)
		(setq *BANDERAASESINA* 1)
		(setq *TURNO* (flipBit *TURNO*))
	    )
        )
      )
      NIL
  )
)
(defun volarIA (posicionIni posicionFin)
  "<DESTRUCTIVA> Mueve la pieza de la posicionIni a la posicionFin."
      (if (and (equal 1 *VUELOIA*) ;;Si es la tercera etapa para el agente jugador, y...
	       (equal (nth posicionIni *TABLERO*) (nth *TURNO* *JUGADORAS*)) ;;En la posición inicial está una pieza de la jugadora en turno, y...
	       (not (or (equal 'A (nth posicionFin *TABLERO*)) (equal 'B (nth posicionFin *TABLERO*)) )) ;;La posición final está libre de piezas de jugadoras, y...
	       (equal 0 *BANDERAASESINA*) ;;La bandera asesina no está activada...
	  )             ;;Entonces...
	  (progn
	    (setf (nth posicionIni *TABLERO*) posicionIni)
	    (setf (nth posicionFin *TABLERO*) (nth *TURNO* *JUGADORAS*))
	    (if (gana? *TABLERO* *PIEZASHUMANA* 0)
		(progn
		  (setq *ETAPA* -1)
		)
		(if (es3EnLinea? posicionFin *TABLERO*)
		    (progn
		      (setq *BANDERAASESINA* 1)
		      )
		    (progn
		      (setq *TURNO* (flipBit *TURNO*))
		    )
		)
	    )
	  )
	  NIL
      )
)
(defun agenteJugador ()
  "<DESTRUCTIVA> Decide, usando NegaMax, la mejor jugada y la aplica."
  (let (
	(jugada NIL)
	(jugadaAsesina NIL)
	(prof 3)
       )
	(if (equal *ETAPA* 1) ;;Primero checamos en qué etapa se encuentra la agente jugadora
	    (progn
	      (setq jugada (NegaMax-AlphaBeta (listaEstado *TABLERO*) 0 prof most-negative-fixnum  most-positive-fixnum 1 0 *OPSCOLOCAR* 0)) ;;Y luego hacemos NegaMax según esa etapa
	      (if (numberp (nth (second jugada) *TABLERO*))
		  (colocarIA (second jugada))
		  (loop for i from 0 to 23 do
		       (if (numberp (nth i *TABLERO*)) (progn (colocarIA i) (setq jugada (list i i)) (return))  )
		  )
	       )
	      (if (equal 1 *BANDERAASESINA*)
		  (progn
		    (loop for i in (obtenerPosiciones 'A *TABLERO*) do
			 (if (and *BANDERAASESINA*
			          (not (es3EnLinea? i *TABLERO*))
			     )
			     (progn
			       (matarIA i)
			       (setq jugadaAsesina i)
			       (setq *BANDERAASESINA* 0)
			       (imprimeJuego 1 jugada 1 i)
			       (return)
			     )
			 )
		    )
		    ;(setq jugadaAsesina (NegaMax-AlphaBeta (listaEstado *TABLERO*) 0 prof most-negative-fixnum  most-positive-fixnum 1 0 (obtenerPosiciones 'A *TABLERO*) (first (obtenerPosiciones 'A *TABLERO*) )))
		    ;(matarIA (second jugadaAsesina))
		    ;(imprimeJuego 1 jugada 1 jugadaAsesina)
		  )
		  (imprimeJuego 1 jugada NIL -1)
	      )
	    )
	    (if (equal *VUELOIA* 1)
		(progn
		  (setq jugada (NegaMax-AlphaBeta (listaEstado *TABLERO*) 0 1 most-negative-fixnum  most-positive-fixnum 1 '(0 1) *OPSVOLAR* 0)) ;;(mandando diferentes operadores -Aquí la profundidad es 1 porque son muchos operadores
		  (volarIA (first (second jugada)) (second (second jugada)))
		  (if (equal 1 *BANDERAASESINA*)
		      (progn
			(loop for i in (obtenerPosiciones 'A *TABLERO*) do
			     (if (and *BANDERAASESINA*
			              (not (es3EnLinea? i *TABLERO*))
				 )
				 (progn
				   (matarIA i)
				   (setq jugadaAsesina i)
				   (setq *BANDERAASESINA* 0)
				   (imprimeJuego 1 jugada 1 i)
				   (return)
				 )
			     )
			)
			;(setq jugadaAsesina (NegaMax-AlphaBeta (listaEstado *TABLERO*) 0 1 most-negative-fixnum  most-positive-fixnum 1 0 (obtenerPosiciones 'A *TABLERO*) (first (obtenerPosiciones 'A *TABLERO*))))
			;(matarIA (second jugadaAsesina))
			;(imprimeJuego 1 jugada 1 jugadaAsesina)
		      )
		      (imprimeJuego 3 jugada NIL -1)
		  )
	        )
		(progn
		  (if (third *HEURÍSTICASAUX*)
		      (progn
			(if (equal 0 (nth 0 *JUGADADEMATANZA*))
			    (progn
			      (if (heurísticaDeMatanzaInstantánea) (setf (nth 0 *JUGADADEMATANZA*) 1))
			    )
			)
			(if (and (equal 1 (nth 0 *JUGADADEMATANZA*)) (equal 'B (nth (first (second *JUGADADEMATANZA*)) *TABLERO*) ) (numberp (nth (second (second *JUGADADEMATANZA*)) *TABLERO*) ))
			    (progn
			      (setq jugada (list 1 (second *JUGADADEMATANZA*)))
			      (setf (nth 0 *JUGADADEMATANZA*) 2)
			    )
			    (if (and (equal 2 (nth 0 *JUGADADEMATANZA*)) (equal 'B (nth (first (third *JUGADADEMATANZA*)) *TABLERO*) ) (numberp (nth (second (third *JUGADADEMATANZA*)) *TABLERO*) ))
				(progn
				  (setq jugada (list 1 (third *JUGADADEMATANZA*)))
				  (setf (nth 0 *JUGADADEMATANZA*) 0)
				)
				(setq jugada (NegaMax-AlphaBeta (listaEstado *TABLERO*) 0 prof most-negative-fixnum  most-positive-fixnum 1 '(0 1) *OPSMOVER* 0)) ;;para cada etapa)
	       		    )
			)
		      )
		  )
		  (moverIA (first (second jugada)) (second (second jugada)))
		  (if (equal 1 *BANDERAASESINA*)
		      (progn
			(loop for i in (obtenerPosiciones 'A *TABLERO*) do
			     (if (and *BANDERAASESINA*
			              (not (es3EnLinea? i *TABLERO*))
				 )
				 (progn
				   (matarIA i)
				   (setq jugadaAsesina i)
				   (setq *BANDERAASESINA* 0)
				   (imprimeJuego 1 jugada 1 i)
				   (return)
				 )
			     )
			)
			;(setq jugadaAsesina (NegaMax-AlphaBeta (listaEstado *TABLERO*) 0 1 most-negative-fixnum  most-positive-fixnum 1 0 (obtenerPosiciones 'A *TABLERO*) (first (obtenerPosiciones 'A *TABLERO*))))
			;(matarIA (second jugadaAsesina))
			;(imprimeJuego 2 jugada 1 jugadaAsesina)
		      )
		      (imprimeJuego 2 jugada NIL -1)
		  )
		)
	    )
        )
  )
)
;;============================================================================================================================================================================================
;;============================================================================================================================================================================================


(defun reinicia ()
  "<DESTRUCTIVA> Reinicia todos los valores del juego para un nuevo juego."
  (setq *TABLERO* '(00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
  (setq *PIEZASHUMANA* 9)
  (setq *PIEZASIA* 9)
  (setq *TURNO* 0)
  (setq *CEMENTERIO* '(- - - - - - - - - - - - - - - - - -))
  (setq *BANDERAASESINA* 0)
)
(defun iniciaJuego ()
  "<DESTRUCTIVA> Llama a la función 'reinicio' para dar inicio a un nuevo juego."
  (reinicia)
  (format t "Bienvenida al juego 'El molino de los 9', jugando contra una inteligencia artificial.~%")
  (format t "Para iniciar, indique cuáles de las siguientes heurísticas auxiliares desea implementar a la agente jugadora:~%Ingrese en la terminal '(habilitar <V1> <V2> <V3>)'~%Donde <V1>, <V2> y <V3> son valores de verdad para identificar si desea o no activar las heurísticas.~%<V1> activa o desactiva la Heurística Asesina.~%<V2> activa o desactiva la Heurística de Salto de Turno.~%<V3> activa o desactiva la Heurística de Matanza Instantánea.~%")
)

(defun colocar (posicion)
  "<DESTRUCTIVA> Coloca la pieza del jugador en turno en la posición recibida si no está ocupada."
  (if (and (equal 1 *ETAPA*) ;;Si es la primera etapa, y...
	   (not (equal 'A (nth posicion *TABLERO*))) ;;No hay ninguna pieza
	   (not (equal 'B (nth posicion *TABLERO*))) ;;colocada en ese lugar ya, y...
	   (or (> *PIEZASIACOLOCAR* 0)     ;;Hay suficientes piezas
	       (> *PIEZASHUMANACOLOCAR* 0) ;;para colocar, y...
	   )
	   (equal 0 *BANDERAASESINA*) ;;La bandera asesina no está activada...
      )           ;;Entonces...
      (progn
	(setf (nth posicion *TABLERO*) (nth *TURNO* *JUGADORAS*))
	(setq *PIEZASHUMANACOLOCAR* (- *PIEZASHUMANACOLOCAR* 1))
	(if (equal 0 *PIEZASIACOLOCAR*) (setq *ETAPA* 2))
	(if (gana? *TABLERO* *PIEZASIA* 1)
	    (progn
	      (setq *ETAPA* 0)
	      (imprimeJuego -1 -1 NIL -1)
	    )
	    (if (es3EnLinea? posicion *TABLERO*)
		(progn
		  (setq *BANDERAASESINA* 1)
		  (imprimeJuego -1 -1 NIL -1)
	        )
		(progn
		  (setq *TURNO* (flipBit *TURNO*))
		  (agenteJugador)
		)
	    )
	)
      )
      NIL
  )
)
(defun matar (posicion)
  "<DESTRUCTIVA> Elimina la pieza del enemigo en turno en la posición recibida, reduciendo el número de piezas enemigas en su contador."
  (if (and (equal 1 *BANDERAASESINA*) ;;Si la bandera asesina está activada, y...
           (equal (nth posicion *TABLERO*) (nth (flipBit *TURNO*) *JUGADORAS*) ) ;;En la posición dada está una pieza enemiga, y...
	   (not (es3EnLinea? posicion *TABLERO*)) ;;Dicha posición no está en un 3 en línea...
      )                    ;;Entonces...
      (progn
	(setq *CEMENTERIO* (reverse *CEMENTERIO*))
	(pop *CEMENTERIO*)
	(setq *CEMENTERIO* (reverse *CEMENTERIO*))
	(setq *PIEZASIA* (- *PIEZASIA* 1))
	(if (= 3 *PIEZASIA*) (setq *VUELOIA* 1)) ;;Si le quedan exactamente 3 piezas restantes al agente jugador, este entra en la etapa 3.
        (push 'B *CEMENTERIO*)
	(setq *BANDERAASESINA* 0)
	(setf (nth posicion *TABLERO*) posicion)
	(setq *TURNO* (flipBit *TURNO*))
	(format t "Éxito en el asesinato.~%")
        (if (gana? *TABLERO* *PIEZASIA* 1)
	    (progn
	      (setq *ETAPA* 0)
	      (imprimeJuego -1 -1 NIL -1)
	    )
	    (agenteJugador)
	)
      )
      NIL
  )
)
(defun mover (posicionIni posicionFin)
  "<DESTRUCTIVA> Mueve la pieza de la posicionIni a la posicionFin."
  (if (and (equal 2 *ETAPA*) ;;Si es la segunda etapa, y...
	   (equal (nth posicionIni *TABLERO*) (nth *TURNO* *JUGADORAS*)) ;;En la posición inicial está una pieza de la jugadora en turno, y...
	   (not (or (equal 'A (nth posicionFin *TABLERO*)) (equal 'B (nth posicionFin *TABLERO*)) )) ;;La posición final está libre de piezas de jugadoras, y...
	   (esAdyacente? posicionIni posicionFin) ;;La posición final es adyacente a la posición inicial, y...
	   (equal 0 *BANDERAASESINA*) ;;La bandera asesina no está activada...
      )             ;;Entonces...
      (progn
	(setf (nth posicionIni *TABLERO*) posicionIni)
	(setf (nth posicionFin *TABLERO*) (nth *TURNO* *JUGADORAS*))
	(if (gana? *TABLERO* *PIEZASIA* 1)
	    (progn
	      (setq *ETAPA* 0)
	      (imprimeJuego -1 -1 NIL -1)
	    )
	    (if (es3EnLinea? posicionFin *TABLERO*)
		(progn
		  (setq *BANDERAASESINA* 1)
		  (imprimeJuego -1 -1 NIL -1)
	        )
		(progn
		  (setq *TURNO* (flipBit *TURNO*))
		  (agenteJugador)
		)
	    )
	)
      )
      NIL
  )
)
(defun volar (posicionIni posicionFin)
  "<DESTRUCTIVA> Mueve la pieza de la posicionIni a la posicionFin."
      (if (and (equal 1 *VUELOHUMANA*) ;;Si es la tercera etapa para la humana, y...
	       (equal (nth posicionIni *TABLERO*) (nth *TURNO* *JUGADORAS*)) ;;En la posición inicial está una pieza de la jugadora en turno, y...
	       (not (or (equal 'A (nth posicionFin *TABLERO*)) (equal 'B (nth posicionFin *TABLERO*)) )) ;;La posición final está libre de piezas de jugadoras, y...
	       (equal 0 *BANDERAASESINA*) ;;La bandera asesina no está activada...
	  )             ;;Entonces...
	  (progn
	    (setf (nth posicionIni *TABLERO*) posicionIni)
	    (setf (nth posicionFin *TABLERO*) (nth *TURNO* *JUGADORAS*))
	    (if (gana? *TABLERO* *PIEZASIA* 1)
		(progn
		  (setq *ETAPA* 0)
		  (imprimeJuego -1 -1 NIL -1)
		)
		(if (es3EnLinea? posicionFin *TABLERO*)
		    (progn
		      (setq *BANDERAASESINA* 1)
		      (imprimeJuego -1 -1 NIL -1)
		      )
		    (progn
		      (setq *TURNO* (flipBit *TURNO*))
		      (agenteJugador)
		    )
		)
	    )
	  )
	  NIL
      )
)
(iniciaJuego)
