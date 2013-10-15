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
  ([] (rest (fb2 1 nil 2 4)))
  ([n p c3 c5]
     (cons p (lazy-seq (apply fb2 (inc n)
                              (match [c3 c5]
                                     [0  0] [ "fizzbuzz"  2        4]
                                     [0  _] [ "fizz"      2        (dec c5)]
                                     [_  0] [ "buzz"      (dec c3) 4]
                                     :else  [ n           (dec c3) (dec c5)]))))))



(defn fb3
  ([] (rest (fb3 1 0 [nil 2] [nil 4])))
  ([n np [p3 c3] [ p5 c5]]
     (cons (str p3 p5 (when-not (or p3 p5) np))
           (lazy-seq (fb3 (inc n) n
                          (if (zero? c3) ["fizz" 2] [nil (dec c3)])
                          (if (zero? c5) ["buzz" 4] [nil (dec c5)]))))))



(defn fb4 []
  (map (fn [n] (match [(mod n 3) (mod n 5)]
                      [0 0] "fizzbuzz"
                      [0,_] "fizz"
                      [0,_] "buzz"
                      :else n)) (iterate inc 1)))

(defn fb5 [] 
  (map #(apply str (or (seq (str (get ["fizz"] (mod % 3))
                                 (get ["buzz"] (mod % 5))))
                       [%]))
       (iterate inc 1)))
