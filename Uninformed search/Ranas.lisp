;;;======================================================================================
;;;  Ranas.lisp
;;;      Resuelve el problema de las ranas en el estanque con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con tres letras a la izquierda de un cero y a la derecha. 
;;;
;;;              Estado incial:        Estado meta:
;;;              (A B C 0 X Y Z)      (X Z Y 0 C A B) <- La solución exacta que dio el profesor.
;;;                                   (X Y Z 0 A B C) <- Otro posible estado meta
;;;
;;;              Donde: A, B y C = Ranas verdes, X, Y y Z = Ranas cafés, 0 = Espacio vacío
;;;      Dr. Salvador Godoy C. -> Editado por Uriel García Rivas
;;;  enero, 2010 -> marzo, 2019
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos
(defparameter  *tiempoInicial* 0)
(defparameter  *tiempoFinal* 0)
(defparameter  *nodosCreados* 0)
(defparameter  *nodosExpandidos* 0)
(defparameter  *longitudFrontera* 0)
(defparameter  *ops*  '(
			 (:A1 1) ;;Realmente no importa nada el segundo valor (el numérico), solo nos importa
                         (:A2 2) ;;la etiqueta, que nos dice hasta dónde va a saltar la rana A, B, C, X, Y o Z
                         (:A3 3)
                         (:B1 1)
			 (:B2 2)
                         (:B3 3)
			 (:C1 1)
			 (:C2 2)
                         (:C3 3)
                         (:X1 1)
			 (:X2 2)
                         (:X3 3)
                         (:Y1 1)
			 (:Y2 2)
                         (:Y3 3)
                         (:Z1 1)
			 (:Z2 2)
                         (:Z3 3)
                       ))

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria
(defun flip (bit)  (boole  BOOLE-XOR  bit  1))
;;;=======================================================================================
;;  getPos (estado  var)  
;;      estado - Un estado del problema a resolver (sistema)...
;;         var - Una letra que represente a una rana (A, B, C, X, Y o Z)
;;;=======================================================================================
(defun getPos (estado var)
  (loop for k from 0 to 6 do
       (cond
	 ((equal var (nth k estado)) (return-from getPos k)) ;;Devuelve la posición de la rana requerida
       )
  )
)
;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
    (if (> (length *open*) *longitudFrontera*) (setq *longitudFrontera* (length *open*))) ;;Obtiene la longitud máxima de la frontera de decisión
    (pop  *open*)
)

;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado]
;;;=======================================================================================
(defun  valid-operator? (op estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [<e1> <e2> <e3> <e4> <e5> <e6> <e7>], (e, diminutivo de 'espacio')
     el operador tiene estructura : [<etiqueta-humana> <número-cualquiera>]"  
    (let  (
	       (AA  (getPos estado 'A)) ;;Obtenemos las posiciones actuales de todas las ranas
	       (BB  (getPos estado 'B))
	       (CC  (getPos estado 'C))
   	       (XX  (getPos estado 'X))
	       (YY  (getPos estado 'Y))
	       (ZZ  (getPos estado 'Z))
	       (operador (first op)) ;; este operador es la etiqueta humana del operador...
	  )
       (case operador ;;Para que un operador sea válido el espacio al que se va a saltar tiene que estar vacío (por ejemplo, A1 salta '0' posiciones, por lo tanto, AA + 1 tiene que estar vacío, o sea, ser igual a 0)
	    (:A1 (if (< 6 (+ AA 1) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ AA 1) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:A2 (if (< 6 (+ AA 2) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ AA 2) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:A3 (if (< 6 (+ AA 3) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ AA 3) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:B1 (if (< 6 (+ BB 1) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ BB 1) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:B2 (if (< 6 (+ BB 2) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ BB 2) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:B3 (if (< 6 (+ BB 3) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ BB 3) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:C1 (if (< 6 (+ CC 1) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ CC 1) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:C2 (if (< 6 (+ CC 2) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ CC 2) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:C3 (if (< 6 (+ CC 3) ) (return-from valid-operator? NIL) (if (eql 0 (nth (+ CC 3) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:X1 (if (> 0 (- XX 1) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- XX 1) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:X2 (if (> 0 (- XX 2) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- XX 2) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:X3 (if (> 0 (- XX 3) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- XX 3) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:Y1 (if (> 0 (- YY 1) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- YY 1) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:Y2 (if (> 0 (- YY 2) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- YY 2) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:Y3 (if (> 0 (- YY 3) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- YY 3) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:Z1 (if (> 0 (- ZZ 1) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- ZZ 1) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:Z2 (if (> 0 (- ZZ 2) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- ZZ 2) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (:Z3 (if (> 0 (- ZZ 3) ) (return-from valid-operator? NIL) (if (eql 0 (nth (- ZZ 3) estado)) (return-from valid-operator? T) (return-from valid-operator? NIL) ) ))
	    (T "error")
       )
    )
)  


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Para asegurarme de que el estado fuera válido decidí hacer que checara si estaba cada rana una sola vez en todo el estanque.
;;;=======================================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura: [<e1> <e2> <e3> <e4> <e5> <e6> <e7>], (e, diminutivo de 'espacio')"
    (let (
	  (AAA 0);;Contador del número de veces que se tiene a 'A' en el estanque
	  (BBB 0)
	  (CCC 0)
	  (XXX 0)
	  (YYY 0)
	  (ZZZ 0)
	  (contador2 0) ;;Contador del número de veces que se tiene al espacio vacío en el estanque
    	  )
      (if (null estado) (return-from valid-state? NIL))
      (loop for i in estado do
	   (if (equal i 0) (setq contador2 (+ 1 contador2)))
	   (if (equal i 'A) (setq AAA (+ 1 AAA)))
	   (if (equal i 'B) (setq BBB (+ 1 BBB)))
	   (if (equal i 'C) (setq CCC (+ 1 CCC)))
	   (if (equal i 'X) (setq XXX (+ 1 XXX)))
	   (if (equal i 'Y) (setq YYY (+ 1 YYY)))
	   (if (equal i 'Z) (setq ZZZ (+ 1 ZZZ)))
      )
      (if (not (equal contador2 1)) (return-from valid-state? NIL));;Verificamos que cada uno
      (if (not (equal AAA 1)) (return-from valid-state? NIL));;solamente se
      (if (not (equal BBB 1)) (return-from valid-state? NIL));;repita
      (if (not (equal CCC 1)) (return-from valid-state? NIL));;una
      (if (not (equal XXX 1)) (return-from valid-state? NIL));;vez
      (if (not (equal YYY 1)) (return-from valid-state? NIL));;..
      (if (not (equal ZZZ 1)) (return-from valid-state? NIL));;..
      (return-from valid-state? T)
    )
)

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================



(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  (
	       (listaRetorno (list (first estado) (second estado) (third estado) (fourth estado) (fifth estado) (sixth estado) (seventh estado)))
	       (A  (getPos estado 'A))
	       (B  (getPos estado 'B))
	       (C  (getPos estado 'C))
   	       (X  (getPos estado 'X))
	       (Y  (getPos estado 'Y))
	       (Z  (getPos estado 'Z))
	       (operador (first op)) ;; este operador es la etiqueta humana del operador...
	   )
	(case operador
	    (:A1 (if (< 6 (+ A 1) ) (return-from apply-operator NIL) (progn (setf (nth A listaRetorno) 0) (setf (nth (+ A 1) listaRetorno) 'A) (return-from apply-operator listaRetorno)))  );;Hacemos que la rana 'A' salte
	    (:A2 (if (< 6 (+ A 2) ) (return-from apply-operator NIL) (progn (setf (nth A listaRetorno) 0) (setf (nth (+ A 2) listaRetorno) 'A) (return-from apply-operator listaRetorno)))  )
	    (:A3 (if (< 6 (+ A 3) ) (return-from apply-operator NIL) (progn (setf (nth A listaRetorno) 0) (setf (nth (+ A 3) listaRetorno) 'A) (return-from apply-operator listaRetorno)))  )
	    (:B1 (if (< 6 (+ B 1) ) (return-from apply-operator NIL) (progn (setf (nth B listaRetorno) 0) (setf (nth (+ B 1) listaRetorno) 'B) (return-from apply-operator listaRetorno)))  )
	    (:B2 (if (< 6 (+ B 2) ) (return-from apply-operator NIL) (progn (setf (nth B listaRetorno) 0) (setf (nth (+ B 2) listaRetorno) 'B) (return-from apply-operator listaRetorno)))  )
	    (:B3 (if (< 6 (+ B 3) ) (return-from apply-operator NIL) (progn (setf (nth B listaRetorno) 0) (setf (nth (+ B 3) listaRetorno) 'B) (return-from apply-operator listaRetorno)))  )
	    (:C1 (if (< 6 (+ C 1) ) (return-from apply-operator NIL) (progn (setf (nth C listaRetorno) 0) (setf (nth (+ C 1) listaRetorno) 'C) (return-from apply-operator listaRetorno)))  )
	    (:C2 (if (< 6 (+ C 2) ) (return-from apply-operator NIL) (progn (setf (nth C listaRetorno) 0) (setf (nth (+ C 2) listaRetorno) 'C) (return-from apply-operator listaRetorno)))  )
	    (:C3 (if (< 6 (+ C 3) ) (return-from apply-operator NIL) (progn (setf (nth C listaRetorno) 0) (setf (nth (+ C 3) listaRetorno) 'C) (return-from apply-operator listaRetorno)))  )
	    (:X1 (if (> 0 (- X 1) ) (return-from apply-operator NIL) (progn (setf (nth X listaRetorno) 0) (setf (nth (- X 1) listaRetorno) 'X) (return-from apply-operator listaRetorno)))  )
	    (:X2 (if (> 0 (- X 2) ) (return-from apply-operator NIL) (progn (setf (nth X listaRetorno) 0) (setf (nth (- X 2) listaRetorno) 'X) (return-from apply-operator listaRetorno)))  )
	    (:X3 (if (> 0 (- X 3) ) (return-from apply-operator NIL) (progn (setf (nth X listaRetorno) 0) (setf (nth (- X 3) listaRetorno) 'X) (return-from apply-operator listaRetorno)))  )
	    (:Y1 (if (> 0 (- Y 1) ) (return-from apply-operator NIL) (progn (setf (nth Y listaRetorno) 0) (setf (nth (- Y 1) listaRetorno) 'Y) (return-from apply-operator listaRetorno)))  )
	    (:Y2 (if (> 0 (- Y 2) ) (return-from apply-operator NIL) (progn (setf (nth Y listaRetorno) 0) (setf (nth (- Y 2) listaRetorno) 'Y) (return-from apply-operator listaRetorno)))  )
	    (:Y3 (if (> 0 (- Y 3) ) (return-from apply-operator NIL) (progn (setf (nth Y listaRetorno) 0) (setf (nth (- Y 3) listaRetorno) 'Y) (return-from apply-operator listaRetorno)))  )
	    (:Z1 (if (> 0 (- Z 1) ) (return-from apply-operator NIL) (progn (setf (nth Z listaRetorno) 0) (setf (nth (- Z 1) listaRetorno) 'Z) (return-from apply-operator listaRetorno)))  )
	    (:Z2 (if (> 0 (- Z 2) ) (return-from apply-operator NIL) (progn (setf (nth Z listaRetorno) 0) (setf (nth (- Z 2) listaRetorno) 'Z) (return-from apply-operator listaRetorno)))  )
	    (:Z3 (if (> 0 (- Z 3) ) (return-from apply-operator NIL) (progn (setf (nth Z listaRetorno) 0) (setf (nth (- Z 3) listaRetorno) 'Z) (return-from apply-operator listaRetorno)))  )
	    (T "error")
	 )
    )
)


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (setq *nodosExpandidos* (+ *nodosExpandidos* 1))
    (let (
            (descendientes  nil)
	    (nuevo-estado  nil)
	  )
          (dolist  (op  *Ops*  descendientes) 
	    (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
	    (setq *nodosCreados* (+ *nodosCreados* 1))
		 (when (and
			    (valid-operator?  op  estado)           ;; se valida el resultado...
			    (valid-state?  nuevo-estado)
		       )
	               (setq  descendientes  (cons  (list nuevo-estado op) descendientes))
		 )
	  )
     )
)



;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [<e1> <e2> <e3> <e4> <e5> <e6> <e7>],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))


(defun  display-solution (lista-nodos)
  "Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Nodos creados: ~A~%" *nodosCreados*)
    (format  t  "Nodos expandidos: ~A~%" *nodosExpandidos*)
    (format  t  "Longitud máxima de la frontera de búsqueda: ~A~%" *longitudFrontera*)
    (format  t  "Tiempo para encontrar la solución: ~A segundos~%" (/ (- *tiempoFinal* *tiempoInicial*) internal-time-units-per-second))
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))

    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo))
	      )
	 )
    )
)  ;; imprimir el número de paso, operador y estado...

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq  *tiempoInicial* 0)
     (setq  *tiempoFinal* 0)
     (setq  *nodosCreados* 0)
     (setq  *nodosExpandidos* 0)
     (setq  *longitudFrontera* 0)
)


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (setq *tiempoInicial* (get-internal-real-time))
  (let (
	  (nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil)
       )
      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo     (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		 estado   (second  nodo)               ;;Identificar el estado y operador que contiene
		 operador (third  nodo))             
	   (push nodo *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond ((equal  edo-meta  estado)
		                (setq *tiempoFinal* (get-internal-real-time))
		                (format  t  "Éxito.~%")
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		 (t (setq  *current-ancestor*  (first  nodo)) 
		    (setq  sucesores  (expand estado))
		    (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
		    (loop for  element  in  sucesores  do
		      (insert-to-open  (first element)  (second element)  metodo)
		    )
		 )
	   )
      )
  )
)
			     
     
;;;=======================================================================================
;;;=======================================================================================
