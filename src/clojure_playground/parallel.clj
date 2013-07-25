(ns clojure-playground.parallel
  (:require [ clojure.core.async :as async :refer [<!! >!! timeout chan alt!!]] )
)

; don't use me
(defn merge-explode [[h1 & t1 :as l1] [h2 & t2 :as l2]] 
    (cond (nil? h1) l2
          (nil? h2) l1
          (> h1 h2) (cons h2 (merge-explode l1 t2))
          :else     (cons h1 (merge-explode l2 t1))))

;; This version should conserve stack and cpu
(defn merge-lists [l1 l2]
  (loop [[h1 & t1 :as l1] l1
         [h2 & t2 :as l2] l2
         acc              ()]
    (cond (nil? h1) (concat (reverse acc) l2)
          (nil? h2) (concat (reverse acc) l1)
          :else (let [[h l t] (if (> h1 h2) [h2 l1 t2] [h1 l2 t1])]
                  (recur l t (cons h acc))))))

;; Familiar merge sort
(defn msort [l] (if (< (count l) 2) l
                    (let [[l1 l2] (split-at (-> l count (/ 2) int) l)]
                      (merge-lists (msort l1) (msort l2)))))
; (msort (repeatedly 10 rand)

;; Generalize associative reduce
(defn assoc-reduce [in merge & {:keys [enrich impoverish]
                                :or   {enrich identity impoverish identity}}]
  (letfn [(assoc-reduce* [l]
             (if (< (count l) 2) (enrich l)
                 (let [[l1,l2] (split-at (int (/ (count l) 2)) l)]
                   (merge (assoc-reduce* l1) (assoc-reduce* l2) ))))]
    (impoverish (assoc-reduce* in))))
; (assoc-reduce (repeatedly 10 rand) merge-lists)

(defn merge-running-sums [rs1 rs2]
  (let [r0 (last rs1)]
    (concat rs1 (map (partial + r0) rs2))
))
;(assoc-reduce (range 10) merge-running-sums)


;; More complicated example: calculate ROLLING sum
;; Knit together two rolling sum objects, which look like [left-fringe average right-fringe]
(defn knit [m
            [ll la lr :as l]
            [rl ra rr :as r]]
  (let [weave (concat lr rl)
        boxcar (inc (* 2 m))]
    (if (>= (count weave) boxcar)
      (let [cs  (reverse (reduce #(conj %1 (+ (peek %1) %2)) '(0) weave))
            cbig   (drop boxcar cs)
            csmall (drop-last boxcar cs)
            ca     (map #(float (/ (- %1 %2) boxcar)) cbig csmall)
            ml     (- (* m 2) (count ll))
            mr     (- (* m 2) (count rr))]
        (list (concat ll (take ml rl))
              (concat la ca ra)
              (concat (take-last mr lr) rr))
        )
      [weave [] weave]
      )))

(defn enrich-rolling [l] (let [x (first l)] [[x],[],[x]]))
(defn impoverish-rolling [m [l a r]] (concat (take m l) a (take-last m r)))
;(assoc-reduce (range 10) (partial knit 1) :enrich enrich-rolling :impoverish  (partial impoverish-rolling 1) )

;; Parallelize with futures
(defn par-assoc-reduce [in merge & {:keys [enrich impoverish timeout-ms] 
                                    :or   {enrich     identity
                                           impoverish identity
                                           timeout-ms 1000}}]
  (letfn [(assoc-reduce* [l]
            (if (< (count l) 2) (future (enrich l))
                (let [[l1,l2] (split-at (int (/ (count l) 2)) l)]
                  (future
                    (let [[f1,f2] (map assoc-reduce* [l1 l2])]
                      (merge @f1 @f2))))))]
    (impoverish (deref (assoc-reduce* in) timeout-ms "timed-out"))))

#_(
  (par-assoc-reduce (repeatedly 10 rand) merge-lists)
  (time (count (par-assoc-reduce (repeatedly 1000 rand) merge-lists)))
  (time (count (assoc-reduce (repeatedly 1000 rand) merge-lists))) 
  (time (count (par-assoc-reduce (repeatedly 10000 rand) merge-lists))) 
)

;; scala break...


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
(defn avenir-map 
  ([func1 a]
     (let [am (agent nil)]
       (send a (fn [v] 
                 (if v (send am (fn [_] (func1 v)))
                     (let [id (str (java.util.UUID/randomUUID))]
                       (add-watch a id
                                  (fn [k ref o n] (when n (let [res (func1 n)]
                                                            (send am (fn [_] res)))))) ))
                 v))
       am))
  ([funcn a1 a2 & as]
     (let [as   (concat [a1 a2] as)
           abuf (agent 0)
           am   (agent nil)
           id (str (java.util.UUID/randomUUID))]
       ; when abuf reaches number of agents, apply funcn on the deref'd list
       (add-watch abuf id 
                  (fn [k ref o n] (when (= n (count as)) 
                                    (let [res (apply funcn (map deref as))]
                                                           (send am (fn [_] res))))))
                                        ; add watch to each agent, incrementing count in abuf
       (doseq [a as] (avenir-map (fn [v] (when v (send abuf inc))) a))
       am)))


(defmacro avenir
  "Takes a body of expressions and yields an agent which will
   receive the result."
  [& body] `(avenir-call (^{:once true} fn* [] ~@body)))

; wait for the thing to be non-nil, with timeout.  I can't figure out any way to do
; this other than adding a watch to detect the completed calculation and then a latch to
; implement the block.
(defn avenir-await [a timeout-ms]
  (let [latch      (new java.util.concurrent.CountDownLatch 1)
        id         (str (java.util.UUID/randomUUID))]
    (add-watch a id (fn [k ref o n] (when n (.countDown latch)) n))
    (.await latch timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
    (deref a)))

#_(
   (def latch (new java.util.concurrent.CountDownLatch 1))
   (def f (avenir (do (.await latch) (.nextDouble (java.util.Random.)))))
   (def g1 (avenir-map #(str "My favorite number is " % " ") f))
   (def g2 (avenir-map #(str "But mine is " (* % 2) " ") f))
   (def h (avenir-map #(println (str %1 %2)) g1 g2 ))
 )

(defn avenir-assoc-reduce [in merge & {:keys [enrich impoverish minpar timeout-ms] :or
                                           {enrich     identity
                                            impoverish identity
                                            minpar     1
                                            timeout-ms 1000}}]
  (letfn [(assoc-reduce-mt [l]
            (let [n (count l)]
             (condp > n
               2        (avenir (enrich l))
               minpar   (avenir (assoc-reduce-1t l))
                 (let [[l1,l2] (split-at (int (/ n 2)) l)
                       [f1,f2] (map assoc-reduce-mt [l1 l2]) ]
                   (avenir-map merge f1 f2)))))
          (assoc-reduce-1t [l]
            (if (< (count l) 2) (enrich l)
                (let [[l1,l2] (split-at (int (/ (count l) 2)) l)]
                  (merge (assoc-reduce-1t l1) (assoc-reduce-1t l2)))))]
    (impoverish (avenir-await (assoc-reduce-mt in) timeout-ms))))

#_ (
    (time (count (assoc-reduce (repeatedly 100000 rand) merge-lists)))
    (time (count (avenir-assoc-reduce (repeatedly 100000 rand) merge-lists)))
    (time (count (avenir-assoc-reduce (repeatedly 100000 rand) merge-lists :minpar 1000)))
)
