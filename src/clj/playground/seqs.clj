

(def ss [[1 2 3] [4 5 6] [7 8 9]])

(defn sss [] (map #(drop (* 2 %) (range)) (range)))


(defn inv [seqs]
  (for [i (range)]
    (map #(nth % i) seqs)))

(defn inv2 [sss] 
  (map #(map first %)
       (iterate #(map next %) sss)))

(defn inv3 [sss] 
  (map #(map first %)
       (iterate #(for [s %] (next s)) sss)))

(defn nexto [seqs] (map next seqs))

(defn inv4 [ss]
  (map (partial map first)
       (iterate (partial map rest) ss)))


(defn f1 [s]
  (let [_ (println "entering f1")
        r (map first s)]
    (println "leaving f1")
    r))

(defn f2 [s]
  (let [_ (println "entering f2")
        r (map rest s)]
    (println "leaving f2")
    r))

(defn inv5 [ss]
  (map f1 (iterate f2 ss)))

(defn mymap  [f c] (reduce #(concat %1 (list (f %2))) () c))
(defn mymap3 [f coll]
  (cons (f (first coll)) (lazy-seq (mymap3 f (rest coll)))))

(defn mymap2 [f coll]
  (if (seq coll)
    (cons (f (first coll)) (lazy-seq (mymap2 f (rest coll))))
    ()))

(defn inv6 [ss]
  (map (partial mymap3 first)
       (iterate (partial mymap3 rest) ss)))

; Stack overflow
#_(nth (iterate #(map inc %) '(5) ) 100000)

;10005
#_(nth (iterate inc 5) 10000)

; (100005)
#_(nth (iterate (partial mymap inc) '(5) ) 100000)

; out of memory
#_(first (nth (iterate (partial mymap inc) (range) ) 100000))

; (1000 1001 1002 1003 1004)
#_(nth (iterate (partial mymap2 inc) (range 5)) 1000)
; stack overflow
#_(nth (iterate (partial mymap2 inc) (range 5)) 10000)

; 100000
#_(first (nth (iterate (partial mymap3 inc) (range) ) 100000))
; stackoverlflow
#_(second (nth (iterate (partial mymap3 inc) (range) ) 100000))





