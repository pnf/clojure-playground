(ns parallel 
  (:use clojure.test))

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



;;;;;;;;;;;;; tests

(deftest test-mergey
  (is (= (mergey [1 2 2 3 4] [2 2 3 3 3]) [1 2 2 2 2 3 3 3 3 4]))
  (is (= (mergey [] []) []))
  (is (= (mergey [] [1 2 3]) [1 2 3]))
  (is (= (mergey [1 2 3] []) [1 2 3])))

(deftest test-msort 
  (is (= (msort '(3 1 7 4 7 8 1 9 6 2 4 0 5 9 3 0 1 7 8 5))
                '(0 0 1 1 1 2 3 3 4 4 5 5 6 7 7 7 8 8 9 9)))
  (is (= (msort []) [])))


(deftest test-fmsort 
  (is (= (fmsort '(3 1 7 4 7 8 1 9 6 2 4 0 5 9 3 0 1 7 8 5) 100)
                '(0 0 1 1 1 2 3 3 4 4 5 5 6 7 7 7 8 8 9 9)))
  (is (= (fmsort [] 100) [])))
