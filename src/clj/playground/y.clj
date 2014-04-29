(ns clojure-playground.y)

(defn fact [n] (if (= 0 n) 1 (* n (fact (- n 1)))))
(assert (= (fact 5) 120))

;; explicitly pass in the function we recur on
(defn fact2 [fact n] (if (= 0 n) 1 (* n (fact fact (- n 1)))))
(assert (= (fact2 fact2 5) 120))

;; curry it
(defn fact3 [fact]
  (fn [n] (if (= 0 n) 1 (* n ((fact fact) (- n 1)) ))))
(assert (= ((fact3 fact3) 5) 120))

;; pull out the self-application, to make the inner bit prettier
(defn fact4 [fact] (fn [n] 
                     (let [f (fn [g n] (if (= 0 n) 1 (* n (g (- n 1))))) ]
                       (f (fact fact) n)
                       )))
(assert (= ((fact4 fact4) 5) 120))

;; again, curry the function that has two arguments
(defn fact5 [fact] (fn [n] 
                     (let [f (fn [g]
                               (fn [n] (if (= 0 n) 1 (* n (g (- n 1))))))]
                       ((f (fact fact)) n)
                       )))
(assert (= ((fact5 fact5) 5) 120))

;; pull out the f, which doesn't depend on any lexical variables
(def factgen (fn [g] (fn [n] (if (= 0 n) 1 (* n (g (- n 1)))))))

;; now pass f in as an argument
(defn fact6 [f]
  (fn [fact] (fn [n] ((f (fact fact)) n))))
(assert (= (((fact6 factgen) (fact6 factgen)) 5) 120))

;; now clean it up, so we only have to call fact once
(defn fact7 [f]
  (let [g (fn [fact] (fn [n] ((f (fact fact)) n)))] (g g)))
(assert (= ((fact7 factgen) 5) 120))

;; get rid of the let, to make this more schemey
(defn fact8 [f]
  ((fn [g] (g g))
   (fn [fact] (fn [n] ((f (fact fact)) n)))))
(assert (= ((fact8 factgen) 5) 120))

;; rename it
(defn Y [f] ((fn [g] (g g))   (fn [h] (fn [n] ((f (h h)) n))))) 

; Note that, unlike Haskell, this definition will cause a stack overflow.
(defn Y2 [f]  ((fn [g] (g g)) (fn [h] (f (h h)))))

(assert (= ((Y factgen) 5) 120))

(defn rangegen [f] (fn [n] (if (= 0 n 0) () (conj (f (- n 1)) n))))
(assert (= ((Y rangegen) 5) '(5 4 3 2 1)))

