(ns cm2.core
  (:use [scad-clj.scad]
        [scad-clj.model]
        [cm2.utils]
        ))

;; CM-2

(def cm2-hi    63)  ; Total height of Tamiko and a CM-2
(def cubie-len 27)  ; There are eight cubies
(def gut-len    4)  ; The gap between cubbies
(def base-len  54)  ; The length of the base and inner cube
(def base-hi   (- cm2-hi (* 2 cubie-len) gut-len)) ; Base height (below cubies)
(def half-gut  (/ gut-len 2))
(def grill-hi  half-gut)

(def inner-cube
  (cube-translate base-len base-len (- cm2-hi gut-len) [(/ gut-len 2)(/ gut-len 2) 0]))

(def cm2
  (->> (cube cubie-len cubie-len cubie-len)
       (trans3 (/ cubie-len 2))
       (op-copy translate [half-gut half-gut half-gut])
       (op-copy mirror [0 0 1])
       (op-copy mirror [1 0 0])
       (op-copy mirror [0 1 0])
       (translate [0 0 base-hi])
       (trans3 (+ cubie-len half-gut))
       (union inner-cube)))

(def top-grill
  (let [grill-len (- cubie-len gut-len)
        grill-wid 1.5
        grill-sep (* gut-len 0.7)]
    (->>
     (apply union
            (for [i (range 1 (- (/ (/ cubie-len grill-sep) 2) 0))]
              (union
               (->> (cube grill-wid grill-len grill-hi)
                    (translate [(* (- i 1) grill-sep) 0 0])))))
     (op-copy mirror [1 0 0])
     (op-copy rotate  (/ Math/PI  2) [0 0 1])
     (translate [(+ (/ cubie-len 2) half-gut)
                 (+ (/ cubie-len 2) half-gut)
                 (/ grill-hi 2)]))))

(def top-mask
  (->> (hull top-grill)
       (excise (translate [0 0  0.01] top-grill))
       (excise (translate [0 0 -0.01] top-grill))))

(defn quad [q]
  (->> q
       (op-copy mirror [1 0 0])
       (op-copy mirror [0 1 0])))

(def quad-mask  (quad top-mask))

(defn raise-quad [qm]
  (->> qm
       (trans3    (+ cubie-len half-gut))
       (translate [0 0 (+ 0.01 cubie-len base-hi)])))

(def cm2-detailed
  (->> cm2
       (excise (text3D "CM-2" 7 1 :yz :halign "center" :valign "baseline"
                       :shift [(+ base-len gut-len) (/ cubie-len 2) (+ 4 gut-len)]))
       (excise (raise-quad quad-mask))))


(def slide-mask-top
  (->> (cube 70 70 (+ cubie-len gut-len base-hi 2) :center false)
       (translate [-5 -5 -2])
       (union (->> inner-cube
                   (trans3 (- 0 cubie-len half-gut))
                   (scale [1.02 1.02 1])
                   (trans3 (+ 0 cubie-len half-gut))))))

(def slide-mask-bottom
  (->> (cube 70 70 (+ cubie-len gut-len base-hi 2) :center false)
       (translate [-5 -5 -2.01])
       (union (->> inner-cube
                   (trans3 (- 0 cubie-len half-gut))
                   (scale [1 1 1])
                   (trans3 (+ 0 cubie-len half-gut))))))

(def cavity
  (let [cav-thick 1.5 ; thickness of the wall after the cavity is removed
        cav-len   (- base-len (* 2 cav-thick))
        cav-hi    (- cm2-hi gut-len (* 2 cav-thick))
        cav-shift (+ half-gut cav-thick)]
    (cube-translate cav-len cav-len cav-hi [cav-shift cav-shift cav-thick])))

(def slide-top
  (->> cm2-detailed
       (excise slide-mask-top)))

(def slide-bottom
  (->> cm2-detailed
       (intersection slide-mask-bottom)
       (excise (translate [0 0 1.6] cavity))))
