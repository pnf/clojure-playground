(ns clojure-playground.parallel-test
  (:require [clojure.test :refer :all]
            [clojure-playground.parallel :refer :all]))

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
