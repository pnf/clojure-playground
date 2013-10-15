(ns clojure-playground.core)

(ns my-great-project.core
  "This namespace is CRAZY!"
   (:use [clojure.string :only [split join]] :reload)
   (:require clojure.stacktrace
             [clojure.test :as test]
             (clojure template walk) :verbose)
   (:import (java.util Date GregorianCalendar)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
