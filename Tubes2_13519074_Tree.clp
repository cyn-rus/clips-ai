;;; TEMPLATES
(deftemplate tree
    (slot root)
    (slot nodeL)
    (slot nodeR)
)

(deftemplate constraint
    (slot attribute)
    (slot value)
)

(deftemplate question
    (multislot attribute)
    (slot text)
)

(deftemplate result
    (slot number)
    (slot text)
)

;;; METHODS
(defmethod get-answer ((?question STRING))
    (printout t ?question " ")
    (bind ?answer (read))
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
    (while (or (not (numberp ?answer)) (< ?answer 0)) do
        (printout t ?question " ")
        (bind ?answer (read))
        (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
    ?answer
)

;;; FACTS
(deffacts tree
    (tree (root mean_concave_points)  (nodeL worst_radius)         (nodeR worst_perimeter))
    (tree (root worst_radius)         (nodeL radius_error)         (nodeR mean_texture_right))
    (tree (root radius_error)         (nodeL worst_texture_left)   (nodeR mean_smoothness))
    (tree (root worst_texture_left)   (nodeL 1)                    (nodeR worst_area))
    (tree (root worst_area)           (nodeL 1)                    (nodeR mean_radius_left))
    (tree (root mean_radius_left)     (nodeL mean_texture_left)     (nodeR 1))
    (tree (root mean_texture_left)    (nodeL 0)                    (nodeR 1))
    (tree (root mean_smoothness)      (nodeL 1)                    (nodeR 0))
    (tree (root mean_texture_right)   (nodeL 1)                    (nodeR concave_points_error))
    (tree (root concave_points_error) (nodeL 0)                    (nodeR 1))
    (tree (root worst_perimeter)      (nodeL worst_texture_right)  (nodeR 0))
    (tree (root worst_texture_right)  (nodeL worst_concave_points) (nodeR perimeter_error))
    (tree (root worst_concave_points) (nodeL 1)                    (nodeR 0))
    (tree (root perimeter_error)      (nodeL mean_radius_right)    (nodeR 0))
    (tree (root mean_radius_right)    (nodeL 0)                    (nodeR 1))
)

(deffacts constraints
    (constraint (attribute mean_concave_points)  (value 0.05))
    (constraint (attribute worst_radius)         (value 16.83))
    (constraint (attribute radius_error)         (value 0.63))
    (constraint (attribute worst_texture_left)   (value 30.15))
    (constraint (attribute worst_area)           (value 641.6))
    (constraint (attribute mean_radius_left)     (value 13.45))
    (constraint (attribute mean_texture_left)    (value 28.79))
    (constraint (attribute mean_smoothness)      (value 0.09))
    (constraint (attribute mean_texture_right)   (value 16.19))
    (constraint (attribute concave_points_error) (value 0.01))
    (constraint (attribute worst_perimeter)      (value 114.45))
    (constraint (attribute worst_texture_right)  (value 25.65))
    (constraint (attribute worst_concave_points) (value 0.17))
    (constraint (attribute perimeter_error)      (value 1.56))
    (constraint (attribute mean_radius_right)    (value 13.34))
)

(deffacts questions  
    ; CONCAVE POINT
    (question   (attribute mean_concave_points)                     (text "Mean concave point?"))
    (question   (attribute concave_points_error)                    (text "Concave points error?"))
    (question   (attribute worst_concave_points)                    (text "Worst concave points?"))
    ; PERIMETER
    (question   (attribute perimeter_error)                         (text "Perimeter error?"))  
    (question   (attribute worst_perimeter)                         (text "Worst perimeter?"))
    ; AREA
    (question   (attribute worst_area)                              (text "Worst area?"))
    ; TEXTURE
    (question   (attribute mean_texture_left mean_texture_right)     (text "Mean texture?"))
    (question   (attribute worst_texture_left worst_texture_right)  (text "Worst texture?"))
    ; RADIUS
    (question   (attribute mean_radius_left mean_radius_right)      (text "Mean radius?"))
    (question   (attribute radius_error)                            (text "Radius error?"))
    (question   (attribute worst_radius)                            (text "Worst radius?"))  
    ; SMOOTHNESS
    (question   (attribute mean_smoothness)                         (text "Mean smoothness?"))
)

(deffacts result
    (result (number 0) (text "Terprediksi Tidak Kanker Payudara"))
    (result (number 1) (text "Terprediksi Kanker Payudara"))
)

;;; RULE
(defrule start
    =>
    (assert (phase search))
    (assert (node mean_concave_points))
)

(defrule ask-question
    (phase search)
    ?attribute <- (node ?currNode)
    (question (attribute $? ?currNode $?) (text ?text))
    =>
    (bind ?value (get-answer ?text))
    (assert (phase predicting))
    (assert (currValue ?value))
)

(defrule predict
    ?phase <- (phase predicting)
    ?inputValue <- (currValue ?userValue)
    ?attr <- (node ?currAttr)
    (tree (root ?currAttr) (nodeL ?nodeLeft) (nodeR ?nodeRight))
    (constraint (attribute ?currAttr) (value ?constraintValue))
=>
    (retract ?phase ?attr)
    (
        if (<= ?userValue ?constraintValue) then 
            (assert (node ?nodeLeft))    
        else 
            (assert (node ?nodeRight))
    )
)

(defrule predictionResult
    ?prediction <- (node ?node)
    (result (number ?node) (text ?text))
    ?phase <- (phase search)
    (or
        (test (= ?node 1))
        (test (= ?node 0))
    )
=>
    (retract ?phase)
    (printout t ?text crlf)
)
