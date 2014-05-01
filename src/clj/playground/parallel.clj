(ns playground.parallel
  (:require [ clojure.core.async :as async :refer [<! >! <!! timeout chan alt!! go close!]] )
(:use [clojure.algo.monads
         :only (domonad with-monad m-lift m-seq m-reduce m-when
                sequence-m
                maybe-m
                state-m fetch-state set-state 
                writer-m write
                cont-m run-cont call-cc
                maybe-t)]))



;; Merge sort is the poster child for associative reduction
(defn merge-lists [l1 l2]
  (loop [[h1 & t1 :as l1] l1
         [h2 & t2 :as l2] l2
         acc              ()]
    (cond (nil? h1) (concat (reverse acc) l2)
          (nil? h2) (concat (reverse acc) l1)
          :else (let [[h l t] (if (> h1 h2) [h2 l1 t2] [h1 l2 t1])]
                  (recur l t (cons h acc))))))

(defn msort [l] (if (< (count l) 2) l
                    (let [[l1 l2] (split-at (-> l count (/ 2) int) l)]
                      (merge-lists (msort l1) (msort l2)))))
; (msort (repeatedly 10 rand))





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
        latch
          |
          f       wait on the latch, then emit random double
        /   \
      g1    g2    embed the double in a string
        \   /
          h       combine the strings
          |

   (def latch (new java.util.concurrent.CountDownLatch 1))
   (def f (avenir (do (.await latch) (.nextDouble (java.util.Random.)))))
   (def g1 (avenir-map #(str "My favorite number is " % " ") f))
   (def g2 (avenir-map #(str "But mine is " (* % 2) " ") f))
   (def h (avenir-map #(println (str %1 %2)) g1 g2 ))

   (.countDown latch)
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

; Execute in a go block and try to put the result on the channel repeatedly, forever.
; Would be nice to stop when this was closed...
(defn zukunft-call [func0]
  (let [c (chan 1)]
    (go (let [res (func0)] (while (not @(.closed c)) (>! c res))))
    c))

(defn zukunft-map  
  "Apply f to the output of z-in and coninually republish on returned z-out.
When called with multiple input channels, wait for all of them to deliver before
appying f.
"
  ([f z-in]
     (let [z-out (chan 1)]
       ; wait on input channel
       (go (let [r (f (<! z-in))]
             ; keep broadcasting until input closed
             (while (not @(.closed z-in)) (>! z-out r))
             (close! z-out)))
       z-out))
  ([f z1 z2 & zs]
     (let [zs    (concat [z1 z2] zs)
           z-out (chan 1)
           rs    (atom {})
           done  (chan 1)]
       ; wait for all inputs in parallel
       (doall (for [z-in zs] 
                (go (let [r (<! z-in)]
                      (swap! rs #(assoc % z-in r))
                      (when (= (count @rs) (count zs))
                        (let [r   (apply f (map @rs zs))]
                          (while (not (.closed @(first zs))) (>! z-out r))
                          (close! z-out))
                        (>! done 1))))))
       z-out)))

(defmacro zukunft
  "Takes a body of expressions and yields a channel, which will publish the results
until closed.
"
  [& body] `(zukunft-call (^{:once true} fn* [] ~@body)))

; Note that we don't need an await anymore.

#_(
        latch
          |
          f       wait on the latch, then emit random double
        /   \
      g1    g2    embed the double in a string
        \   /
          h       combine the strings
          |

   (def latch (new java.util.concurrent.CountDownLatch 1))
   (def f (zukunft (do (.await latch) (.nextDouble (java.util.Random.)))))
   (def g1 (zukunft-map #(str "My favorite number is " % " ") f))
   (def g2 (zukunft-map #(str "But mine is " (* % 2) " ") f))
   (def h (zukunft-map #(println (str %1 %2)) g1 g2 ))
   (go (println (<! h)))
   (.countDown latch)
 )


; cbind :: Chan c => c a -> (a -> c b) -> c b
(defn cbind [c f]
  (let [c2 (chan)]
    (go (let [x (or  (<! c) [])]
          (close! c)
          (>! c2 (<! (apply f x)))))
    c2))

(defn cb-fn->chan-fn [f & args1]
  (fn chan-fn [& args2]
    (let [c    (chan)
          cb   (fn [& res] (go (>! c res)  #_(close! c)) nil)
          args (concat (or (seq  args1) args2) [cb])]
      (apply f args)
      c)))

(defn fake-mkdir [dir cb] (let [c (timeout 2000)]
                            (go (<! c)
                                (println "made" dir)
                                (cb dir)))
  true)

