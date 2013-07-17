(ns clojure-playground.parallel)

(defn mergey [[h1 & t1 :as l1] [h2 & t2 :as l2]] 
  (do 
    ;(println "Entering mergey with " l1 l2)  (Thread/sleep 100)
    (cond (nil? h1) l2
          (nil? h2) l1
          (> h1 h2) (cons h2 (mergey l1 (rest l2)))
          :else     (cons h1 (mergey l2 (rest l1))))
    )
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


