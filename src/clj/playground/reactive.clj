(ns playground.reactive)


(defn- sully
  "Set dirty bit at k and in all dependents"
  [dag k]
  (loop [dag                  dag
         [[k depth] & stack]  (list [k 0])
         seen                 #{}]
    (cond 
     (not k)  dag
     (or (get-in dag [k :dirty]) (seen k)) (recur dag stack seen)
     true     (recur (if (get-in dag [k :function])  (assoc-in dag [k :dirty] true) dag)
                     (concat (map vector
                                  (get-in dag [k :deps] #{})
                                  (repeat (inc depth)))
                             stack)
                     (conj seen k)))))

(defn- dispossess 
  "Remove dependency on us, if we're a function."
  [dag k]
  (if-let [kargs (get-in dag [k :args])]
    (reduce (fn [dag karg] (update-in dag [karg :deps] #(disj % k))) dag kargs))
  dag)

(defn set-val* 
  "Set a regular value here, and clean up this was previously a function.  E.g. (set-val* :a 30)"
  [dag k v]
  (-> dag
      (dispossess k)
      (assoc-in [k :value] v)
      (sully k)))

(defmacro set-val
  "Set a regular value here, and clean up this was previously a function.  E.g. (set-val a 30)"
  [dag k v]
  `(set-val* ~dag ~(keyword k) ~v))

(defn ensure-val* [dag k]
  (let [node        (get dag k)
        function    (get node :function)
        dirty       (get node :dirty)]
    (if (and function dirty)
      (-> dag
          (as-> % (reduce ensure-val* % (node :args)))
          (as-> % (assoc-in % [k :value] (function %)))
          (assoc-in [k :dirty] false))      
      dag)))

(defmacro ensure-val [dag k] `(ensure-val* ~dag ~(keyword k)))

(defn get-val* [dag k]  (get-in (ensure-val* dag k) [k :value]))
(defmacro get-val [dag k] `(get-val* ~dag ~(keyword k)))

(defn- set-deps [dag kf kargs]
  (reduce (fn [dag k] (update-in dag [k :deps] #(if % (conj % kf) #{kf}))) dag kargs))

(defn set-fn* [dag k kargs f]
  (-> dag
        (assoc-in [k :args] kargs)
        (assoc-in [k :dirty] true)
        (sully k)
        (set-deps k kargs)
        (assoc-in [k :function] f)))


(defmacro set-fn [dag k args & forms]
  (let [kargs    (map #(keyword %) args)
        vs       (map (fn [k] `(get-in ~'dag [~k :value])) kargs)
        bindings (mapcat list args vs)]
    `(set-fn* ~dag ~(keyword k) [~@kargs] (fn [~'dag] (let [~@bindings] ~@forms)))))

(defn pridentity [x]
  (println x)
  x)

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
