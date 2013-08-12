(ns clojure-playground.y)

(defn fact [n] (if (= 0 n) 1 (* n (fact (- n 1)))))
;(fact 5)

;; explicitly pass in the function we recur on
(defn fact2 [fact n] (if (= 0 n) 1 (* n (fact fact (- n 1)))))
;(fact2 fact2 5)

;; curry it
(defn fact3 [fact]
  (fn [n] (if (= 0 n) 1 (* n ((fact fact) (- n 1)) ))))
; ((fact3 fact3) 5)


;; pull out the self-application, to make the inner bit prettier
(defn fact4 [fact] (fn [n] 
                     (let [f (fn [g n] (if (= 0 n) 1 (* n (g (- n 1))))) ]
                       (f (fact fact) n)
                       )))
;((fact4 fact4) 5)

;; again, curry the function that has two arguments
(defn fact5 [fact] (fn [n] 
                     (let [f (fn [g]
                               (fn [n] (if (= 0 n) 1 (* n (g (- n 1))))))]
                       ((f (fact fact)) n)
                       )))
;((fact5 fact5) 5)

;; pull out the f, which doesn't depend on any lexical variables
(def factgen (fn [g]
           (fn [n] (if (= 0 n) 1 (* n (g (- n 1)))))))

(defn fact6 [fact] (fn [n] 
                     ((factgen (fact fact)) n)))
;((fact6 fact6) 5)

;; now pass f in as an argument
(defn fact7 [f]
  (fn [fact] (fn [n] ((f (fact fact)) n))))
;(((fact7 factgen) (fact7 factgen)) 5)

;; now clean it up, so we only have to call fact once
(defn fact8 [f]
  (let [g (fn [fact] (fn [n] ((f (fact fact)) n)))] (g g)))
((fact8 factgen) 5)

;; get rid of the let, to make this more schemey
(defn fact9 [f]
  ((fn [g] (g g))
   (fn [fact] (fn [n] ((f (fact fact)) n)))))
;((fact8 factgen) 5)

;; rename it
(defn Y [f]
  ((fn [g] (g g))
   (fn [h] (fn [n] ((f (h h)) n)))))
