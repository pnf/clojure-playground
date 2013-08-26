(ns clojure-playground.types
 (:require 
  [clojure.core.typed :refer [ann check-ns tc-ignore dotimes> def-alias typed-deps]]))

(ann foo [Number->Number])
(defn foo [x] (+ 1 x))
(defn bleh [] (foo "notanumber"))

(defn -main [& args]
  (println "first")
  ;(println (foo (first args)))
)

;(check-ns)

