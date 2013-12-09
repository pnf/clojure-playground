(ns playground.monads
  (:use [clojure.algo.monads
         :only (domonad with-monad m-lift m-seq m-reduce m-when
                sequence-m
                maybe-m
                state-m fetch-state set-state 
                writer-m write
                cont-m run-cont call-cc
                maybe-t)]))

(domonad sequence-m
         [x (range 5)
          y (range 3)]
         (+ x y))

(with-monad sequence-m  ; will define m-bind and m-result locally
  (def bleh (m-result 1))
  (defn inco [n] (m-result (inc n)))
  (defn multo [n] (m-result (* 2 n)))
  (assert (= (m-bind (m-bind (m-result 1) multo) inco)
             '(3)))
)

(assert (= '(3)
         (domonad sequence-m
                  [x (m-result 1)
                   y (inco x)   ; rhs equivalent to (m-bind (m-result 1) inco)
                   z (inco y)]  ; rhs equivalent to (m-bind (m-bind (m-result 1) inco) inco)
                  z)))

(defn inco2 [n] (list (inc n)))
(assert (= '(6)
           (inco 5)
           (inco2 5)
           ))

(domonad sequence-m
         [x [1 2 3]
          y ['a 'b 'c]]
         [x y])  ; [1 a] etc. combinations

; How can I do the equivalent directly?
(with-monad sequence-m
  (m-bind [1 2 3] (fn [_] [1 2 3]))    ; 1 2 3 1 2 3 1 2 3
  (m-bind [1 2 3] (fn [_] ['a 'b 'c])) ; a b c a b c a b c
;  (fn [x1 x2] )

)

