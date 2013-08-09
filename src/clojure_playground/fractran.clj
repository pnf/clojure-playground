(ns clojure-playground.fractran
  ( :require [clojure.math.numeric-tower :as nt]
             [clojure.string :as cs]))

;; Suppose John Conway invented his own version of brainfuck.
;; He did.
;;
;; A fractran program is defined as an integer input and a sequence of rational fractions.
;; The program is evaluated as follows:
;;    1.  Find the first fraction f in the series such that n*f is an integer.
;;    2.  If there isn't one, stop.  The result is n.
;;    3.  Let n = n*f
;;    4.  Repeat
;;
;; How it works:
;; Every prime number p constitutes a register.
;; You encode a set of register values exponents of those primes:
;;          2^r2 * 3^r3 * 5^r5 * 7^r7 * 11^r11 * etc.
;; A fraction essentially tests whether n's exponents are greater than or equal to
;; those in the denominator.  If the test is successful, then we decrement n's
;; registers by the exponents in the denominator and increment them by those found
;; in the numerator. E.g. if
;;        n    = 36 = 2^2 * 3^2
;;        prog = (3/2)
;; Then
;;         2^2 * 3^2 * 3^1 & 2^-1 = 2^1 * 3^3
;; I.e. we decrement r2 and increment r3.  If you repeat this, r3 ends up containing the
;; sum of the original values of r2 and r3.
;;


(defn fractran-seq [n prog]
  (let [nf (some (fn [nf] (and (integer? nf) nf)) (map (partial * n) prog))]
    (if nf (cons nf (lazy-seq (fractran-seq nf prog))) (list))
))
(defn fractran [n prog] (last (fractran-seq n prog)))

;;;;;;;;;;;;;;;;;;;;;
;; Now some utilities, so we can understand our code.
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

;; bit arithmetic doesn't work for bigint
(defn power-of-2 [n]
  (loop [n n i 0]
    (cond (even? n) (recur (/ n 2) (inc i))
          (= 1 n) i
          :else nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(defn adder [a b]
  (let [prog [3/2]
        n    (* (nt/expt 2 a) (nt/expt 3 b))
        res  (fractran n prog)]
    (( prime-factors res) 3)))


(defn halve [n] 
  (let [prog [15/2 1/45 1/15]
        n    (nt/expt 2 n)
        res  (fractran n prog)]
    ((prime-factors res) 5)
    )
)


;;;; For every power of 2 in the output, that power is a prime.
(defn conway-primes [n]
  (let [prog '(17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1)]
    (take n (filter identity (map power-of-2 (fractran-seq 2 prog))))
))



;; Hello, world!
;; It's been suggested that you could encode text in a single number as
;;      2^(c1 +256*c2 +256*256*c3 + ...)
;; But if we allow the output to be extracted from the sequence of n's, there's an
;; easier way.  The rule will be that every even number in the sequence, the
;; exponent of 2 is an ASCII value.

;; Cryptic notes to self:
;       ki       j    -j           where i=0..m-1  j = m-i = m..1
;      -ki      -j    j-1
(defn text-to-code [s] 
  (mapcat (fn [k j] [(* (nt/expt 2 k)     (nt/expt 3 j)     (nt/expt 5 (- j)))
                     (* (nt/expt 2 (- k)) (nt/expt 3 (- j)) (nt/expt 5 (- j 1)))])
          (map int (seq s)) (range (count s) 0 -1)))



(defn hello-world []
  (let [n    1220703125
        prog '(7528977498068181366035447808/1220703125 244140625/7528977498068181366035447808 1347363005271780922721618896336453632/244140625 48828125/1347363005271780922721618896336453632 57487488224929319369455739577022021632/48828125 9765625/57487488224929319369455739577022021632 19162496074976439789818579859007340544/9765625 1953125/19162496074976439789818579859007340544 51099989533270506106182879624019574784/1953125 390625/51099989533270506106182879624019574784 115422332637413376/390625 78125/115422332637413376 9393093476352/78125 15625/9393093476352 484503604463601835673437673472185597952/15625 3125/484503604463601835673437673472185597952 630864068311981556866455304000241664/3125 625/630864068311981556866455304000241664 1682304182165284151643880810667311104/625 125/1682304182165284151643880810667311104 8762000948777521623145212555558912/125 25/8762000948777521623145212555558912 11408855402054064613470328848384/25 5/11408855402054064613470328848384 25769803776/5 1/25769803776)
        res (fractran-seq n prog)
        
        ]
(apply str (map char (filter identity (map #((prime-factors %) 2) res))))
))
