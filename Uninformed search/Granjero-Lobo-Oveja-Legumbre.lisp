;;;======================================================================================
;;;  Granjero-Lobo-Oveja-Legumbre.lisp
;;;      Resuelve el problema del Granjero, Lobo, Oveja y Legumbre con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla, un bit que representa si está (1) o no (0) el Granjero (G), Lobo (Lo), Oveja (O), Legumbre (Le).
;;;                     Estado incial:                       Estado meta:
;;;                 G  Lo O  Le   G  Lo O  Le         G  Lo O  Le   G  Lo O  Le
;;;               ((1  1  1  1)  (0  0  0  0))      ((0  0  0  0)  (1  1  1  1))
;;;
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
(defparameter  *ops*  '( (:Nada     (1 0)) ;;Como con el problema de las ranas, aquí no importa realmente
                         (:Lobo     (2 0)) ;;el segundo valor (la lista), solo nos importa la etiqueta, que
                         (:Oveja    (1 1)) ;;nos dice qué bit se va a prender o apagar
                         (:Legumbre (0 2))     
                       ))

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria
(defun flip (bit)  (boole  BOOLE-XOR  bit  1))
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
      (pop  *open*))

;;;=======================================================================================
;;  BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
     (if  (= 1 (first (first  estado)))  0  1))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<G0><Lo0><O0><Le0>) (<G1><Lo1><O1><Le1>)],
     el operador tiene estructura : [<etiqueta-humana> <número-cualquiera>]"  
  (let* (
	 (orilla (nth (barge-shore estado) estado))
	 (operador (first op)) ;; este operador es la etiqueta humana del operador...
	)
         (case operador 
	    (:Nada     T ) 
	    (:Lobo     (if (equal 1 (second orilla)) T NIL) );;Verifica que se encuentre un lobo, oveja o legumbre en la orilla donde se encuentra el granjero
	    (:Oveja    (if (equal 1 (third orilla)) T NIL) ) 
	    (:Legumbre (if (equal 1 (fourth orilla)) T NIL) ) 
	    (T "error")
	 )
  )
  
)  


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en c/orilla hay igual o mayor numero de misioneros que de canibales
;;;=======================================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<G0><Lo0><O0><Le0>) (<G1><Lo1><O1><Le1>)]"
    (let (
          (orilla (nth (flip (barge-shore estado)) estado))
	 )
      (not (or
	    (equal orilla '(0 1 1 1))  ;;Estos son
    	    (equal orilla '(0 1 1 0))  ;;todos los
	    (equal orilla '(0 0 1 1))  ;;estados inválidos
	   )
      )
    )
)

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================



(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  (
	       (orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (g0       (first orilla1))
   	       (lo0      (second orilla1))
	       (o0       (third  orilla1))
	       (le0      (fourth orilla1))
	       (g1       (first  orilla2))
	       (lo1      (second  orilla2))
	       (o1       (third   orilla2))
	       (le1      (fourth orilla2)) 
	       (operador (first op)) ;; este operador es la etiqueta humana del operador...
	   )
	 (case operador 
	    (:Nada     (list (list (flip g0) lo0 o0 le0) (list (flip g1) lo1 o1 le1) ) ) 
	    (:Lobo     (list (list (flip g0) (flip lo0) o0 le0) (list (flip g1) (flip lo1) o1 le1) ) )
	    (:Oveja    (list (list (flip g0) lo0 (flip o0) le0) (list (flip g1) lo1 (flip o1) le1) ) ) 
	    (:Legumbre (list (list (flip g0) lo0 o0 (flip le0)) (list (flip g1) lo1 o1 (flip le1)) ) ) 
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
     el estado tiene estructura:  [(<G0><Lo0><O0><Le0>) (<G1><Lo1><O1><Le1>)],
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
