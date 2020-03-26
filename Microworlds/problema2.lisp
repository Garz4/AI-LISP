;;=====================================================================
;;   Conjunto de reglas para el Problema 1.
;;       Uriel García Rivas, Instituto Politécnico Nacional
;;=====================================================================
(defrule start          ;;Inicialización de las capibaras.
	:group
			:initialization
	:when
			T
	:do
		        (set-entities :herbivore 3 :desert)
		        (set-entities :carnivore 3 :desert)
		        (set-entities :scavenger 3 :desert)
)

(defrule eating-herbivores
	:group
		:carnivores
	:when	
		(< (get-entity-food @id) (/ (get-entity-food @id) 2))
		(view-field-vision @id1
			(equal  (get-entity-type @id1) :herbivore)
		)
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(feed-entity @id @id1))
(defrule eating-scavengers
	:group
		:carnivores
	:when	
		(< (get-entity-food @id) (/ (get-entity-food @id) 4))
		(view-field-vision @id1
			(equal  (get-entity-type @id1) :scavenger))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(feed-entity @id @id1))
(defrule food-view-scavengers
	:group
		:scavengers
	:when	
		(> (get-entity-movements @id) 1)
		(view-field-vision @id1
			(in (get-entity-type @id1) (get-consumable-type @id))
			(< (manhattan-distance @cell (get-entity-coordinates @id1)) 6))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal)
		(feed-entity @id @id1))



(defrule herd-scavengers
	:group
		:scavengers
	:when	
		(search-cell @cell1
			(not (equal (get-cell-type @cell1) :water))
			(> (get-cell-entity-type-number @cell1 :scavenger) 2))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal))


(defrule eating-herbivores
	:group
		:carnivores
	:when	
		(< (get-entity-food @id) (/ (get-entity-food @id) 2))
		(view-field-vision @id1
			(equal  (get-entity-type @id1) :herbivore)
			(> (get-entity-food @id) 20))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(feed-entity @id @id1))

(defrule emigrate-carnivores
	:group
		:carnivores
	:when	
		(< (get-entity-type-count @id :carnivore) 5)
		(search-distant-cell @cell1
			(equal (get-cell-type @cell1) :grass))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal))
(defrule bebe-agua1          ;;Busca agua y bebe, aunque no tenga sed mortal.
	:group
			:all
	:when	
	                (> (get-entity-movements @id) 0)
	                (<= (get-entity-water @id) 60)
		        (>= (get-entity-food @id) 30)
			(search-cell @cell1
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					(area-around @cell1 :water))
			(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(drink-water @id)
)
(defrule come-algo1           ;;Busca comida y come, aunque no tenga hambre mortal.
	:group
			:herbivores
	:when	
		        (> (get-entity-movements @id) 0)
			(<= (get-entity-food @id) 90)
			(>= (get-entity-water @id) 30)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-consumable-type @id))
					;(> (get-entity-food @id1) 10)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(feed-entity @id @id1)
)
(defrule busca-algo-o-te-vas-a-morir           ;;Cuando se empieza a quedar hambrienta o sedienta la capibara, la obliga a moverse a la izquierda lo que más le da su visión  (para que no se tope con agua).
	:group
			:all
	:when	
	                (> (get-entity-movements @id) 0)
			(or
			             (<= (get-entity-food @id) 60)
				     (<= (get-entity-water @id) 70)
			)
			(search-cell @cell1
				     (equal (get-cell-type @cell1) :desert)
				     (>= (manhattan-distance (get-entity-coordinates @id)  @cell1) 3)
				     (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
			)
			
	:do
	                (move-entity-to @id @cell2 :diagonal)
)

(defrule bebe-agua          ;;Busca agua y bebe, aunque no tenga sed mortal.
	:group
			:herbivores
	:when	
	                (> (get-entity-movements @id) 0)
	                (<= (get-entity-water @id) 60)
			(>= (get-entity-food @id) 30)
			(search-cell @cell1
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					(area-around @cell1 :water))
			(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(drink-water @id)
)
(defrule come-algo           ;;Busca comida y come, aunque no tenga hambre mortal.
	:group
			:herbivores
	:when	
		        (> (get-entity-movements @id) 0)
			(<= (get-entity-food @id) 90)
			(>= (get-entity-water @id) 30)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-consumable-type @id))
					;(> (get-entity-food @id1) 10)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(feed-entity @id @id1)
)
(defrule muevete-que-te-contaminaaaaaaaaaaaaas            ;;Si su última posición es una posición de 
	:group
			:all
	:when	
	                (> (get-entity-movements @id) 0)
		        (equal (get-entity-cell-type @id) :contamination)
			(search-cell @cell1
				     (equal (get-cell-type @cell1) :desert)
				     (equal (manhattan-distance (get-entity-coordinates @id)  @cell1) 1)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(feed-entity @id @id1)
)
(defrule busca-algo-o-te-vas-a-morir2           ;;Cuando se empieza a quedar hambrienta o sedienta la capibara, la obliga a moverse a la izquierda lo que más le da su visión  (para que no se tope con agua).
	:group
			:all
	:when	
	                (> (get-entity-movements @id) 0)
			(or
			             (<= (get-entity-food @id) 60)
				     (<= (get-entity-water @id) 70)
			)
			(search-cell @cell1
				     (equal (get-cell-type @cell1) :desert)
				     (>= (manhattan-distance (get-entity-coordinates @id)  @cell1) 3)
				     (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
			)
			
	:do
	                (move-entity-to @id @cell2 :diagonal)
)
(defrule tengan-hijitos
	:group
		        :all
	:when	
		        (>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		        (>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
	:do	
		        (reproduce-entity @id)
)
;;=====================================================================
