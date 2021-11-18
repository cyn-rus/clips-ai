;;; TEMPLATES
(deftemplate av
  (slot attribute)
  (slot value)
)

(deftemplate question
  (slot attribute)
  (slot text)
)

(deftemplate score
  (slot value)
)

(deftemplate result
  (slot text (default "%nHasil Prediksi = %sTerprediksi Kanker Payudara%n"))
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
(deffacts questions  
  ; CONCAVE POINT
  (question   (attribute mean_concave_point)    (text "Mean concave point?"))
  (question   (attribute concave_points_error)  (text "Concave points error?"))
  (question   (attribute worst_concave_points)  (text "Worst concave points?"))
  ; PERIMETER
  (question   (attribute perimeter_error)       (text "Perimeter error?"))  
  (question   (attribute worst_perimeter)       (text "Worst perimeter?"))
  ; AREA
  (question   (attribute worst_area)            (text "Worst area?"))
  ; TEXTURE
  (question   (attribute mean_texture)          (text "Mean texture?"))
  (question   (attribute worst_texture)         (text "Worst texture?"))
  ; RADIUS
  (question   (attribute mean_radius)           (text "Mean radius?"))
  (question   (attribute radius_error)          (text "Radius error?"))
  (question   (attribute worst_radius)          (text "Worst radius?"))  
  ; SMOOTHNESS
  (question   (attribute mean_smoothness)       (text "Mean smoothness?"))
)

(deffacts result
  (result)
)

;;; RULE
(defrule predict
  (declare (salience -10))
  (score (value ?v))
  (result (text ?t))
  =>
  (if (= ?v 1) then
    (format t ?t "")
  else
    (format t ?t "Tidak ")
  )
)

(defrule ask-question
   (question (attribute ?attribute) (text ?text))
   (not (av (attribute ?attribute)))
   =>
   (bind ?value (get-answer ?text))
   (assert (av (attribute ?attribute) (value ?value)))
)


;;; KNOWLEDGE BASE
;; VERSION 1
(defrule negative
  (av   (attribute mean_concave_point)    (value ?mcp))
  (av   (attribute concave_points_error)  (value ?cpe))
  (av   (attribute worst_concave_points)  (value ?wcp))
  (av   (attribute perimeter_error)       (value ?pe))
  (av   (attribute worst_perimeter)       (value ?wp))
  (av   (attribute worst_area)            (value ?wa))
  (av   (attribute mean_texture)          (value ?mt))
  (av   (attribute worst_texture)         (value ?wt))
  (av   (attribute mean_radius)           (value ?mr))
  (av   (attribute radius_error)          (value ?re))
  (av   (attribute worst_radius)          (value ?wr))
  (av   (attribute mean_smoothness)       (value ?ms))
  (or
    (and
      (test (<= ?mcp 0.05))   (test (<= ?wr 16.83))
      (test (<= ?re 0.63))    (test (> ?wt 30.15))
      (test (> ?wa 641.60))   (test (<= ?mr 13.45))
      (test (<= ?mt 28.79))
    )
    (and
      (test (<= ?mcp 0.05))   (test (<= ?wr 16.83))
      (test (> ?re 0.63))     (test (> ?ms 0.09))
    )
    (and
      (test (<= ?mcp 0.05))   (test (> ?wr 16.83))
      (test (> ?mt 16.19))    (test (<= ?cpe 0.01))
    )
    (and
      (test (> ?mcp 0.05))    (test (<= ?wp 114.45))
      (test (<= ?wt 25.65))   (test (> ?wcp 0.17))
    )
    (and
      (test (> ?mcp 0.05))    (test (<= ?wp 16.83))
      (test (> ?wt 25.65))    (test (<= ?pe 1.56))
      (test (<= ?mr 13.34))
    )
    (and
      (test (> ?mcp 0.05))    (test (<= ?wp 16.83))
      (test (> ?wt 25.65))    (test (> ?pe 1.56))
    )
    (and
      (test (> ?mcp 0.05))    (test (> ?wp 16.83))
    )
  )
  =>
  (assert (score (value 0)))
)

(defrule positive
  (declare (salience -8))
  (not (score (value 0)))
  =>
  (assert (score (value 1)))
)

;; VERSION 2
; (defrule negative-1
;   (av (attribute mean_concave_point) (value ?mcp)) (test (<= ?mcp 0.05))
;   (av (attribute worst_radius) (value ?wr)) (test (<= ?wr 16.83))
;   (av (attribute radius_error) (value ?re)) (test (<= ?re 0.63))
;   (av (attribute worst_texture) (value ?wt)) (test (> ?wt 30.15))
;   (av (attribute worst_area) (value ?wa)) (test (> ?wa 641.60))
;   (av (attribute mean_radius) (value ?mr)) (test (<= ?mr 13.45))
;   (av (attribute mean_texture) (value ?mt)) (test (<= ?mt 28.79))
;   =>
;   (assert (score (value 0)))
; )

; (defrule negative-2
;   (av (attribute mean_concave_point) (value ?mcp)) (test (<= ?mcp 0.05))
;   (av (attribute worst_radius) (value ?wr)) (test (<= ?wr 16.83))
;   (av (attribute radius_error) (value ?re)) (test (> ?re 0.63))
;   (av (attribute mean_smoothness) (value ?ms)) (test (> ?ms 0.09))
;   =>
;   (assert (score (value 0)))
; )

; (defrule negative-3
;   (av (attribute mean_concave_point) (value ?mcp)) (test (<= ?mcp 0.05))
;   (av (attribute worst_radius) (value ?wr)) (test (> ?wr 16.83))
;   (av (attribute mean_texture) (value ?mt)) (test (> ?mt 16.19))
;   (av (attribute concave_points_error) (value ?cpe)) (test (<= ?cpe 0.01))
;   =>
;   (assert (score (value 0)))
; )

; (defrule negative-4
;   (av (attribute mean_concave_point) (value ?mcp)) (test (> ?mcp 0.05))
;   (av (attribute worst_perimeter) (value ?wp)) (test (<= ?wp 114.45))
;   (av (attribute worst_texture) (value ?wt)) (test (<= ?wt 25.65))
;   (av (attribute worst_concave_points) (value ?wcp)) (test (> ?wcp 0.17))
;   =>
;   (assert (score (value 0)))
; )

; (defrule negative-5
;   (av (attribute mean_concave_point) (value ?mcp)) (test (> ?mcp 0.05))
;   (av (attribute worst_perimeter) (value ?wp)) (test (<= ?wp 16.83))
;   (av (attribute worst_texture) (value ?wt)) (test (> ?wt 25.65))
;   (av (attribute perimeter_error) (value ?pe)) (test (<= ?pe 1.56))
;   (av (attribute mean_radius) (value ?mr)) (test (<= ?mr 13.34))
;   =>
;   (assert (score (value 0)))
; )

; (defrule negative-6
;   (av (attribute mean_concave_point) (value ?mcp)) (test (> ?mcp 0.05))
;   (av (attribute worst_perimeter) (value ?wp)) (test (<= ?wp 16.83))
;   (av (attribute worst_texture) (value ?wt)) (test (> ?wt 25.65))
;   (av (attribute perimeter_error) (value ?pe)) (test (> ?pe 1.56))
;   =>
;   (assert (score (value 0)))
; )

; (defrule negative-7
;   (av (attribute mean_concave_point) (value ?mcp)) (test (> ?mcp 0.05))
;   (av (attribute worst_perimeter) (value ?wp)) (test (> ?wp 16.83))
;   =>
;   (assert (score (value 0)))
; )

; (defrule positive
;   (declare (salience -8))
;   (not (score (value 0)))
;   =>
;   (assert (score (value 1)))
; )

;; VERSION 3
;; NEED TO BE FIXED : False Prediction
; (defrule positive
;   (declare (salience -5))
;   (or
;     (and
;       (av (attribute mean_concave_point) (value ?mcp)) (test (<= ?mcp 0.05))
;       (or
;         (and
;           (av (attribute worst_radius) (value ?wr)) (test (<= ?wr 16.83))
;           (or
;             (and
;               (av (attribute radius_error) (value ?re)) (test (<= ?re 0.63))
;               (or
;                 (and (av (attribute worst_texture) (value ?wt)) (test (<= ?wt 30.15)))
;                 (and
;                   (av (attribute worst_texture) (value ?wt)) (test (> ?wt 30.15))
;                   (or
;                     (and (av (attribute worst_area) (value ?wa)) (test (<= ?wa 641.60)))
;                     (and
;                       (av (attribute worst_area) (value ?wa)) (test (> ?wa 641.60))
;                       (or
;                         (and (av (attribute mean_radius) (value ?mr)) (test (> ?mr 13.45)))
;                         (and
;                           (av (attribute mean_radius) (value ?mr)) (test (<= ?mr 13.45))
;                           (av (attribute mean_texture) (value ?mt)) (test (> ?mt 28.79))
;                         )
;                       )
;                     )
;                   )
;                 )
;               )
;             )
;             (and (av (attribute mean_smoothness) (value ?ms)) (test (<= ?ms 0.09)) )
;           )
;         )
;         (and
;           (av (attribute worst_radius) (value ?mcp)) (test (> ?mcp 16.83))
;           (or
;             (and (av (attribute mean_texture) (value ?mt)) (test (<= ?mt 16.19)))
;             (and
;               (av (attribute mean_texture) (value ?mt)) (test (> ?mt 16.19))
;               (av (attribute concave_points_error) (value ?cpe)) (test (> ?cpe 0.01))
;             )
;           )
;         )
;       )
;     )
;     (and
;       (av (attribute mean_concave_point) (value ?mcp)) (test (> ?mcp 0.05))
;       (av (attribute worst_perimeter) (value ?wp)) (test (<= ?wp 114.45))
;       (or
;         (and
;           (av (attribute worst_texture) (value ?wt)) (test (<= ?wt 25.65))
;           (av (attribute worst_concave_points) (value ?wcp)) (test (<= ?wcp 0.17))
;         )
;         (and
;           (av (attribute worst_texture) (value ?wt)) (test (> ?wt 25.65))
;           (av (attribute perimeter_error) (value ?pe)) (test (<= ?pe 1.56))
;           (av (attribute mean_radius) (value ?ma)) (test (> ?ma 13.34))
;         )
;       )
;     )
;   )
;   =>
;   (assert (score (value 1)))
; )

; (defrule negative
;   (declare (salience -8))
;   (not (score (value 1)))
;   =>
;   (assert (score (value 0)))
; )
