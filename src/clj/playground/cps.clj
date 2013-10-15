(ns playground.cps)

(def x 5)
(assert (= (* (inc x) 2)
           ((fn [y] (* y 2)) (inc x))))

; factorial with accumulator
(defn fa [n a] (if (zero? n) a (fa (dec n) (* a n))))
(assert (= 720 (fa 6 1)))

; factorial with accumulator closure.  (Continuation passing.)
(defn fc [n k] (if (zero? n) (k 1) (fc (dec n) (fn [m] (k (* m n))))))
(assert (= 720 (fc 6 identity)))

; Mechanically:
;  (f x) ==> (f x k)
;  expr  ==> (fn [x] (k expr)


; Now curried, for that traditional look.
(defn fcc [n] (fn [k] (if (zero? n) (k 1) (fc (dec n) (fn [m] (k (* m n)))))))
(assert (= 720 ((fcc 6) identity)))


; Conventional fibonnaci sequence
(defn fib [n] (if (< n 2)
                n
                (+ (fib (- n 1)) (fib (- n 2)))))
(assert (= 5 (fib 5)))

; 
(defn fib-cps [n k]
  (if (< n 2)
    (k n)
    (fib-cps (- n 1) (fn [m] (k (+ m (fib-cps1 (- n 2) identity)))))))
(assert (= 5 (fib-cps 5 identity)))

