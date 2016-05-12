(ns cm2.utils
  (:use [scad-clj.scad] [scad-clj.model]))

;; Graphics Utilities

(defn op-copy "union with operated copy" [op & args] (union (last args) (apply op args)))
(defn scale3  "scale all dimensions"     [s q]       (scale [s s s] q))
(defn trans3  "translate all dimensions" [w q]       (translate [w w w] q))
  
(defn excise "like difference, but subtraction is from the last node, not the first"
  [& nodes]
  (difference (last nodes) (drop-last nodes)))

(defn mirror-xy "reflect along plane defined by x=y"
  [q]
  (->> q
       (mirror [1 1 0])
       (rotate Math/PI [0 0 1])))

(defn cube-translate [x y z offset]
  (->> (cube x y z :center false)
       (translate offset)))

(defn text3D "generate bold text, and extrude. Double height to add or subtract"
  [txt siz hi plane
   & {:keys [halign valign angle shift font offset-thick]
      :or {halign "center" valign "center"
           angle 0 shift [0 0 0]
           font "Alright Sans:style=Bold"
           offset-thick nil}}]
  (as-> (text txt :size siz :font font :halign halign :valign valign) $
        (if-not offset-thick $ (difference $ (offset offset-thick $)))
        (rotate angle [0 0 1] $)
        (extrude-linear {:height (* 2 (Math/abs hi)) :center false :convexity 10} $)
        (translate [0 0 (- hi)] $)
        (case plane
          :xy $
          :xz (->> $
                   (rotate (/ Math/PI  2) [1 0 0])
                   (rotate    Math/PI     [0 0 1]))
          :yz (->> $
                   (rotate (/ Math/PI  2) [1 0 0])
                   (rotate (/ Math/PI  2) [0 0 1])))
        (translate shift $)))
(comment
(text3D "Test text 11" 19  2 :xy)
(text3D "Test text 11" 19  2 :xy :offset-thick -1)
(text3D "Test text 12" 19  2 :xy :angle (/ Math/PI 4))
(text3D "Test text 13" 19  2 :xy :angle (/ Math/PI 4) :shift [-30 30 10])
(text3D "Test text 14" 19  6 :xz :halign "left")
(text3D "Test text 15" 19  6 :xz :halign "left"  :angle (/ Math/PI 4))
(text3D "Test text 16" 19 10 :yz :halign "right" :valign "baseline")
(text3D "Test text 16" 19 10 :yz :halign "right" :valign "baseline" :offset-thick -1)
(text3D "Test text 17" 19 10 :yz :halign "right" :valign "baseline" :shift [0 0 30])
)

