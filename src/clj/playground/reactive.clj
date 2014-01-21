(ns playground.reactive
  (:require [clojure.algo.monads :as m])
  (:import (org.apache.commons.math3.special Erf)
           (java.lang Math) ))

#_(
The basic structure of the dag is
  {:keyword {:dirty trueOrFalse
             :deps  #{namesThatDependOnUs}
             :value setOrCalculated
             :function functionOfDagThatReturnsValue
   }}
)


(defn- sully
  "Set dirty bit at k (if this is a function node) and in all dependents (in any case)."
  [dag k]
  ; Stack is a list of [:keyword depth] tuples.
  (loop [dag                  dag
         [[k depth] & stack]  (list [k 0])]
    (cond 
     (not k)  dag
     (get-in dag [k :dirty]) (recur dag stack)
     true     (recur (if (get-in dag [k :function])  (assoc-in dag [k :dirty] true) dag)
                     (concat (map vector
                                  (get-in dag [k :deps] #{})
                                  (repeat (inc depth)))
                             stack)))))

(defn- dispossess 
  "Remove all dependencies on us, if we're a function."
  [dag k]
  (if-let [kargs (get-in dag [k :args])]
    (reduce (fn [dag karg] (update-in dag [karg :deps] #(disj % k))) dag kargs))
  dag)

(defn- set-deps [dag kf kargs]
  (reduce (fn [dag k] (update-in dag [k :deps] #(if % (conj % kf) #{kf}))) dag kargs))



(defn set-val* 
  "Set a regular value here, dirty all dependents, and clean up this was previously a function.  E.g. (set-val* :a 30)"
  [dag k v]
  (-> dag
      (dispossess k)
      (assoc-in [k :value] v)
      (sully k)))

(defn ensure-val*
  "Ensure that a value is available for :k by evaluating all necessary function nodes."
  [dag k]
  (let [node        (get dag k)
        function    (get node :function)
        dirty       (get node :dirty)]
    (if (and function dirty)
      (-> dag
          (as-> % (reduce ensure-val* % (node :args)))
          (as-> % (assoc-in % [k :value] (function %)))
          (assoc-in [k :dirty] false))      
      dag)))


(defn get-val*
  "Retrieve value for :k, ensuring first that it's been calculated if necessary."
  [dag k] 
  (let [dag (ensure-val* dag k)]
    [dag (get-in dag [k :value])]))

(defn set-fn* [dag k kargs f]
  (-> dag
        (assoc-in [k :args] kargs)
        (assoc-in [k :dirty] true)
        (sully k)
        (set-deps k kargs)
        (assoc-in [k :function] f)))

#_(Purty macro version that let us write keywords as if they were symbols.)

(defmacro rfn [args & forms]
  (let [kargs    (map #(keyword %) args)
        vs       (map (fn [k] `(get-in ~'dag [~k :value])) kargs)
        bindings (mapcat list args vs)]
    `(fn [~'dag] (let [~@bindings] ~@forms))))


(defmacro set-fn
  "Set a function node, e.g.
   (set-fn dag calcdNode [param1 param2] (foo param1 param2))"
  [dag k args & forms]
  (let [kargs    (map #(keyword %) args)
        vs       (map (fn [k] `(get-in ~'dag [~k :value])) kargs)
        bindings (mapcat list args vs)]
    `(set-fn* ~dag ~(keyword k) [~@kargs] (fn [~'dag] (let [~@bindings] ~@forms)))))

(defmacro set-val
  "Set a regular value here, and clean up this was previously a function.  E.g. (set-val a 30)"
  [dag k v]
  `(set-val* ~dag ~(keyword k) ~v))

(defmacro ensure-val [dag k] `(ensure-val* ~dag ~(keyword k)))

(defmacro get-val [dag k] `(get-val* ~dag ~(keyword k)))

(defmacro gv [dag k] `(second (get-val* ~dag ~(keyword k))))


#_( All the -s functions are state monads, that is, closure functions from dag to [result dag])

(defn set-val-s* [k val]
  (fn [dag] [nil (set-val* dag k val)]))


(defn set-fn-s* [k args f]
  (fn [dag] [nil (set-fn* dag k args f)]))

(defn get-val-s* [k]
  (fn [dag]
    (let [[dag v] (get-val* dag k)]
      [v dag])))

#_(Pretty versions of the state monads)


(defmacro set-val-s [k v] `(set-val-s* ~(keyword k) ~v))

(defmacro set-fn-s [k args & forms]
  `(set-fn-s* ~(keyword k) ~(vec (map keyword args)) (rfn ~args ~@forms)))

(defmacro get-val-s [k] `(get-val-s* ~(keyword k)))

(defmacro print-val-s
  "Retrieve and print out k, while maintaining the monad chain."
  [k & stuff]
  `(m/with-monad m/state-m (m/m-fmap #(println ~@stuff (str ~(str k) "=" %)) (get-val-s ~k))))
(defn pridentity [x]  (println x)  x)

(defn print-state-s [& stuff]
  (fn [dag] (apply println (concat stuff dag)) [nil dag]))


#_(-> {}
    (set-val :a 1)
    (set-val :b 2)
    pridentity
    (set-fn* :c [:a :b] (fn [dag] (+ (get-in dag [:a :value]) (get-in dag [:b :value]))))
    pridentity
    (ensure-val :c))


#_(-> {}
    (set-val* :a 1)
    (set-val* :b 2)
    pridentity
    (set-fn* :c [:a :b] (fn [dag] (+ (get-in dag [:a :value]) (get-in dag [:b :value]))))
    pridentity
    (ensure-val* :c) (set-val* :a 42) pridentity (ensure-val* :c) pridentity (set-val* :b 8) (ensure-val* :c) pridentity (set-val* :c 5))

#_(-> {}
    (set-val a 1)
    (set-val b 2)
    pridentity
    (set-fn c [a b] (+ a b))
    pridentity
    (ensure-val c) pridentity (set-val a 42) pridentity (ensure-val c) pridentity (set-val b 8) (ensure-val c) pridentity (set-val c 5))

#_((m/domonad m/state-m 
                [_  (set-val-s* :a 1)
                 _  (set-val-s* :b 2)
                 _  (set-fn-s*  :c [:a :b] (rfn [a b] (+ a b)))
                 v  (get-val-s* :c)]
                v) {})

#_((m/domonad m/state-m 
              [_  (set-val-s a 1)
               _  (set-val-s b 2)
               _  (set-fn-s  c [a b] (+ a b))
               v  (get-val-s c)]
              v) {})

#_((m/domonad m/state-m 
              [_  (set-val-s a 1)
               _  (set-val-s b 2)
               _  (set-fn-s c [a b] (+ a b))
	       _  (print-val-s c "a=1, b=2, c=a+b")
               _  (set-fn-s  d [b c] (* b c))
	       _  (print-val-s d "d=b*c")
	       _  (set-val-s a 2)
	       _  (print-val-s c "a=2")
	       _  (print-val-s d "a=2")
	       _  (set-val-s b 5)
	       _  (print-val-s c "b=5")
	       _  (print-val-s d "b=5")
               v  (get-val-s d)
               ]
              v) {})

(defn option []
      (-> {}
          (set-val K 101.0)
          (set-val S 100.0)
          (set-val T 1.0)
          (set-val r 0.01)
          (set-val sigma 0.35)
          (set-fn d1 [S T K r sigma] (/ (+ (Math/log (/ S K))  (* (+ r (/ (* sigma sigma) 2)) T)) (* sigma (Math/sqrt T))))
          (set-fn d2 [d1 T sigma] (- d1 (* sigma (Math/sqrt T))))
          (set-fn c [S T K r d1 d2] (- (* S (N d1)) (* K (Math/exp (* (- r) T)) (N d2))))
          (set-fn p [S T K r d1 d2] (- (* K (Math/exp (* (- r) T)) (N (- d2))) (* S (N (- d1)))))
          (set-fn delta [c S] (/ (- (-> dag (set-val :S (+ S 0.01)) (gv c)) c) 0.01 ))))

(defn delta [option]
  (let [S0 (gv option S)
        c0  (gv option c)
        S1 (+ S0 0.01)
        c1 (-> option (set-val S S1) (gv c))]
    (/ (- c1 c0) 0.01)))
