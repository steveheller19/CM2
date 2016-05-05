(ns cm2.utils
  (:use [scad-clj.scad] [scad-clj.model]))

;; $=>>, is a threading macro focussing on the last argument, binding $ to each expression in turn.
;; All but the first expression is possibly modified as follows: if the expression already contains $,
;; the expression is bound to $ unmodified. However, any expression (other than the initial expression)
;; which does not contain $, $ is inserted into the last position. In the initial impleletation,
;; $ the name is only searched for at the top level, but the search should reach all levels.
;; $=> is a threading macro with $ that focuses on the first argument.
;; The plan is to define a macro generating macro that generates four macros for a given name foo:
;; foo-> and foo->> which binds foo and always insert foo in the first and last arg positions, and
;; foo=> and foo=>> which binds foo, but only inserts foo when foo is totally absent.
(defn add-name-arg-last [name [fun & expr]]
  (if (some #(= name %) expr)
    (concat [fun] expr)
    (concat [fun] expr [name])))
(comment
  (add-name-arg-last '$ '(union bar baz bongo))
  (add-name-arg-last '$ '(union bar $ baz bongo)))

(defn add-name-arg-first [name [fun & expr]]
  (if (some #(= name %) expr)
    (concat [fun]        expr)
    (concat [fun] [name] expr)))
(comment
  (add-name-arg-first '$ '(difference bar baz bongo))
  (add-name-arg-first '$ '(translate [1 1 1] $)))

(defmacro $=>> "Name the threaded value $, inserting into the last arg porition if totally absent"
  [expr & exprs]
  `(let [~'$ ~expr]
     (~'let [~@(interleave (repeat '$)
                         (map (partial add-name-arg-last '$) exprs))]
       ~'$)))

(defmacro $=> "Name the threaded value $, inserting into the first arg porition if totally absent"
  [expr & exprs]
  `(let [~'$ ~expr]
     (~'let [~@(interleave (repeat '$)
                         (map (partial add-name-arg-first '$) exprs))]
      ~'$)))

(comment
(= (macroexpand
    '($=>> table
           (translate [1 1 1])
           (difference $ foo)))
   (macroexpand
    '($=>> table
           (translate [1 1 1] $)
           (difference $ foo)))
   (macroexpand
    '($=> table
          (translate [1 1 1] $)
          (difference foo)))
   (macroexpand
    '($=> table
          (translate [1 1 1] $)
          (difference $ foo)))
   )
)

;; az-> rationalizes as-> putting the name first
(defmacro az->
  "Variant of as-> Binds name to each form in turn, returning the last form"
  [name & forms]
  (if forms
    `(let [~@(interleave (repeat name) forms)]
       ~name)))
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

(comment
  (let [sig (text (char 963) :size 60)]
  (difference sig (offset -2 sig)))
  )

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
  ($=>> (text txt :size siz :font font :halign halign :valign valign)
        (if-not offset-thick $ (difference $ (offset offset-thick $)))
        (rotate angle [0 0 1])
        (extrude-linear {:height (* 2 (Math/abs hi)) :center false :convexity 10})
        (translate [0 0 (- hi)])
        (case plane
          :xy $
          :xz (->> $
                   (rotate (/ Math/PI  2) [1 0 0])
                   (rotate    Math/PI     [0 0 1]))
          :yz (->> $
                   (rotate (/ Math/PI  2) [1 0 0])
                   (rotate (/ Math/PI  2) [0 0 1])))
        (translate shift)))
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

