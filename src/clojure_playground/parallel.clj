(ns clojure-playground.parallel)

(defn mergey [[h1 & t1 :as l1] [h2 & t2 :as l2]] 
    ;(println "Entering mergey with " l1 l2)  (Thread/sleep 100)
    (cond (nil? h1) l2
          (nil? h2) l1
          (> h1 h2) (cons h2 (mergey l1 (rest l2)))
          :else     (cons h1 (mergey l2 (rest l1))))
)

(defn msort [l] (if (< (count l) 2) l
                    (let [[l1 l2] (split-at (-> l count (/ 2) int) l)]
                      (mergey (msort l1) (msort l2)))))

(defn fmsort [l timeout]
  (defn fmsort* [l]
    (if (< (count l) 2) (future l)
        (let [n (-> l count (/ 2) int)
              [l1 l2] (split-at n l)]
          (future 
            (let [[f1 f2] (map fmsort* [l1 l2])]
              (mergey @f1 @f2))))))
  (deref  (fmsort* l) timeout nil))

(defn assoc-reduce [in enrich merge impoverish]
  (letfn [(assoc-reduce* [l]
             (if (< (count l) 2) (enrich l)
                 (let [[l1,l2] (split-at (int (/ (count l) 2)) l)]
                   (merge (assoc-reduce* l1) (assoc-reduce* l2) ))))]
    (impoverish (assoc-reduce* in))))

(defn par-assoc-reduce [in enrich merge impoverish timeout]
  (letfn [(assoc-reduce* [l]
             (if (< (count l) 2) (future (enrich l))
                 (let [[l1,l2] (split-at (int (/ (count l) 2)) l)]
                   (future
                     (let [[f1,f2] (map assoc-reduce* [l1 l2])]
                       (merge @f1 @f2))))))]
    (impoverish (deref (assoc-reduce* in) timeout "timed-out"))))

(defn msort2 [l] (assoc-reduce l identity mergey identity))
(defn fmsort2 [l timeout] (par-assoc-reduce l identity mergey identity timeout))

; Create an agent, and when func0 has executed send the agent a closure to set its return value.
(defn avenir-call [func0]
  (let [a (agent nil)]
    (future (let [res (func0)]
              (send a (fn [_] res))))
    a))

; Take an original agent, a1 and create a new agent, a2.
; Send a message to the a1, adding a watch callback to itself.
; The callback will evaluate func1 on the contents of a1 and send
; a2 a closure to set the results.  (Note that adding a watch can trigger
; the same watch; hence the 'when' check.
(defn avenir-map [func1 a]
  (let [a2 (agent nil)]
    (send a (fn [v] 
              (if v (send a2 (fn [_] (func1 v)))
                  (let [id (str (java.util.UUID/randomUUID))]
                    (add-watch a id
                               (fn [k ref o n] (when n (let [res (func1 n)]
                                                         (send a2 (fn [_] res)))))) ))
              v))
    a2))

(defn avenir-map-multi [funcn & as]
  (let [abuf (agent 0)
        a2   (agent nil)
        id (str (java.util.UUID/randomUUID))]
    ; when abuf reaches number of agents, apply funcn on the deref'd list
    (add-watch abuf id (fn [k ref o n] (when (= n (count as)) (let [res (apply funcn (map deref as))]
                                                                (send a2 (fn [_] res))))))
    ; add watch to each agent, incrementing count in abuf
    (doseq [a as] (avenir-map (fn [v] (when v (send abuf inc))) a))
    a2)
  )

(defmacro avenir
  "Takes a body of expressions and yields an agent which will
   receive the result."
  [& body] `(avenir-call (^{:once true} fn* [] ~@body)))

; wait for the thing to be non-nil, with timeout
(defn avenir-await [a timeout-ms]
  (let [latch      (new java.util.concurrent.CountDownLatch 1)
        id         (str (java.util.UUID/randomUUID))]
    (add-watch a id (fn [k ref o n] (when n (.countDown latch)) n))
    (.await latch timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
    (deref a)))

(defn par-assoc-reduce-avenir [in enrich merge impoverish timeout-ms]
  (letfn [(assoc-reduce* [l]
            (if (< (count l) 2) (avenir (enrich l))
                (let [[l1,l2] (split-at (int (/ (count l) 2)) l)
                      [f1,f2] (map assoc-reduce* [l1 l2])]
                  (avenir-map-multi merge f1 f2))))]
    (impoverish (avenir-await (assoc-reduce* in) timeout-ms))))


(defn amsort2 [l timeout] (par-assoc-reduce-avenir l identity mergey identity timeout))


