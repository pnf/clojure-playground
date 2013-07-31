(ns clojure-playground.fractran
  ( :require [clojure.math.numeric-tower :as nt]
             [clojure.string :as cs]))

;; Now a version that gives us intermediate results
(defn fractran-seq [n prog]
  (let [nf (some (fn [nf] (and (integer? nf) nf)) (map (partial * n) prog))]
    (if nf (cons nf (lazy-seq (fractran-seq nf prog))) (list))
))
(defn fractran [n prog] 
  "Given an input integer n, and a sequence of fractions prog, repeat the following:
1. Find the first fraction f such that n*nf is an integer
2. Set n <- n*nf and continue
Stop if we can't find an integral n*nf"
  (last (fractran-seq n prog)))

;;;;;;;;;;;;;;;;;;;;;
;; Now some utilities.
(defn maintain-non-primes [non-primes]
  "Remove first non-prime entry, and create new entries based on it"
  (let [[n ps]         (first  non-primes)
        xs             (dissoc non-primes n)
        new-non-primes (map (fn [p] [(+ n p)  (cons p (xs (+ n p))) ]) ps)]
    (into xs new-non-primes)))
(defn seive
  ([] (seive (sorted-map) 2))
  ([n] (take n (seive (sorted-map) 2)))
  ([non-primes n]
     (if (= n (first (first non-primes))) ; did we hit the next non-prime?
       (seive (maintain-non-primes non-primes) (+ n 1))
       (cons n (lazy-seq (seive (assoc non-primes (* 2 n) [n]) (+ n 1)))))))

;; prime factorization by division, so we can examine outputaxs
(defn prime-factors [n]
  (into (sorted-map)
        (loop [n       n
               factors []
               [p & ps] (seive)]
          (if (<= n 1) factors
              (let [[nn k] (loop [n n i 0] (if (integer? n) (recur (/ n p) (inc i))
                                               [(* p n) (dec i)]))]
                (recur nn
                       (if (> k 0) (conj factors [p k]) factors)
                       ps )
                )))))

(defn from-prime-factors [pf] (reduce (fn [n [p k]] (* n (nt/expt p k))) 1 pf))

(def letters (assoc (apply hash-map (mapcat #(list %1 %2) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (seive))) \. 1))

;; Create program from AA;BB or AA;. notation
(defn frac-from-notn [s]
  (let [[den num] (cs/split s #";")]
    (/ (reduce #(* %1 (letters %2)) 1 num) (reduce #(* %1 (letters %2)) 1 den))
))
(defn prog-from-notn [s]
  (map frac-from-notn (cs/split s #"\s*#.*\n\s*")))

;; Because fractions in prefix are annoying
(defn prog-from-string [s]
  (let [fractions (filter #(> (count %) 0) (cs/split s #"[\(\), ]"))]
    (map #(apply / (map bigint (cs/split % #"/"))) fractions)
))

;; examples
(defn halve [n] 
  (let [prog [(/ 15 2) (/ 1 45) (/ 1 15)]
        n    (nt/expt 2 n)
        res  (fractran n prog)]
    ((prime-factors res) 5)
    )
)

(defn adder [a b]
  (let [prog [(/ 3 2)]
        n    (* (nt/expt 2 a) (nt/expt 3 b))
        res  (fractran n prog)]
    (( prime-factors res) 3))
)

;; bit arithmetic doesn't work for bigint
(defn power-of-2 [n]
  (loop [n n i 0]
    (cond (even? n) (recur (/ n 2) (inc i))
          (= 1 n) i
          :else nil)))

(defn conway-primes [n]
  (let [prog (prog-from-string "17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1")]
    (take n (filter identity (map power-of-2 (fractran-seq 2 prog))))
))
