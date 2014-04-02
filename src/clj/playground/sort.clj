(ns playground.sort)


(defn lz [xx] 
  (letfn [(eo [[x & xs]] (if x (cons x (eo (rest xs))) nil))]
    [(eo xx) (eo (rest xx))]
    ))

(defn mg [[x1 & xr1 :as xs1]  [x2 & xr2 :as xs2]]
  (println "merging" xs1 "and" xs2)
  (cond (and x1 x2)  (if (< x1 x2) 
                               (conj (mg xr1 xs2) x1)
                               (conj (mg xs1 xr2) x2))
                x1             (conj (mg xr1 nil) x1)
                x2             (conj (mg nil xr2) x2)
                :else          '()))


(defn ms [[x & xr :as xs]]
  (if xr
    (apply mg (map ms (lz xs)))
    (list x)))
