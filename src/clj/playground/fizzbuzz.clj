(ns playground.fizzbuzz
  (:require [clojure.core.match :refer [match]]))


(defn fb
  ([] (rest (fb nil 1 2 4)))
  ([p n c3 c5]
     (cons p (lazy-seq 
                 (cond (and (zero? c3) (zero? c5)) (fb "fizzbuzz" (inc n) 2 4)
                       (zero? c3) (fb "fizz" (inc n) 2 (dec c5))
                       (zero? c5) (fb "buzz" (inc n) (dec c3) 4)
                       :else (fb n (inc n) (dec c3) (dec c5)))))))


(defn fb2
  ([] (rest (fb nil 1 2 4)))
  ([p n c3 c5]
     (cons p (lazy-seq (match [c3 c5]
                              [0  0] (fb "fizzbuzz" (inc n) 2        4)
                              [0  _] (fb "fizz"     (inc n) 2        (dec c5))
                              [_  0] (fb "buzz"     (inc n) (dec c3) 4)
                              :else  (fb n          (inc n) (dec c3) (dec c5)))))))



