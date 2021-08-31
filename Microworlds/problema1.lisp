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

;;=====================================================================
;;   Conjunto de reglas para el Problema 1.
;;       Uriel Garc�a Rivas, Instituto Polit�cnico Nacional
;;=====================================================================
(defrule start          ;;Inicializaci�n de las capibaras.
	:group
			:initialization
	:when
			T
	:do
			(set-entities :herbivore 10 :desert)
)
(defrule bebe-agua1          ;;Busca agua y bebe, aunque no tenga sed mortal.
	:group
			:herbivores
	:when	
	                (> (get-entity-movements @id) 0)
	                (<= (get-entity-water @id) 60)

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
			(not (and
			      (> (get-entity-food @id) 70)
			      (< (get-entity-water @id) 40)
			     )
			)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-consumable-type @id))
					;(> (get-entity-food @id1) 10)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(feed-entity @id @id1)
)
(defrule busca-algo-o-te-vas-a-morir           ;;Cuando se empieza a quedar hambrienta o sedienta la capibara, la obliga a moverse a la izquierda lo que m�s le da su visi�n  (para que no se tope con agua).
	:group
			:herbivores
	:when	
	                (> (get-entity-movements @id) 0)
			(or
			             (<= (get-entity-food @id) 40)
				     (<= (get-entity-water @id) 40)
			)
			(search-cell @cell1
				     (equal (get-cell-type @cell1) :desert)
				     (equal (manhattan-distance (get-entity-coordinates @id)  @cell1) 3)
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
			(not (and
			      (> (get-entity-food @id) 70)
			      (< (get-entity-water @id) 40)
			     )
			)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-consumable-type @id))
					;(> (get-entity-food @id1) 10)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(feed-entity @id @id1)
)
(defrule muevete-que-te-contaminaaaaaaaaaaaaas            ;;Si su �ltima posici�n es una posici�n de 
	:group
			:herbivores
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
(defrule busca-algo-o-te-vas-a-morir2           ;;Cuando se empieza a quedar hambrienta o sedienta la capibara, la obliga a moverse a la izquierda lo que m�s le da su visi�n  (para que no se tope con agua).
	:group
			:herbivores
	:when	
	                (> (get-entity-movements @id) 0)
			(or
			             (<= (get-entity-food @id) 40)
				     (<= (get-entity-water @id) 40)
			)
			(search-cell @cell1
				     (equal (get-cell-type @cell1) :desert)
				     (equal (manhattan-distance (get-entity-coordinates @id)  @cell1) 3)
				     (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
			)
			
	:do
	                (move-entity-to @id @cell2 :diagonal)
)
;;=====================================================================
