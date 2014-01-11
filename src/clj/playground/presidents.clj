(ns playground.presidents
  (:require [clojure.string :as str]
            [clojure.set :as sets])
  (:import (java.lang Math)))

(def winners (set (str/split "washington adams jefferson jefferson madison madison monroe monroe adams jackson jackson vanburen harrison polk taylor pierce buchanan lincoln lincoln grant grant hayes garfield cleveland harrison cleveland mckinley mckinley roosevelt taft wilson wilson harding coolidge hoover roosevelt roosevelt roosevelt roosevelt truman eisenhower eisenhower kennedy johnson nixon nixon carter reagan reagan bush clinton clinton bush bush obama obama" #" ")))

(def losers (sets/difference  (set (str/split "clinton jefferson adams pinckney pinckney clinton king adams jackson adams clay vanburen vanburen clay cass scott fremont breckinridge mcclellan seymour greeley tilden hancock blaine cleveland harrison bryan bryan parker bryan roosevelt hughes cox davis smith hoover landon wilkie dewey dewey stevenson stevenson nixon goldwater humphrey mcgovern ford carter mondale dukakis bush dole gore kerry mccain romney" #" ")) winners ))




(def initial (set (map #(str "^" % "$") winners)))

(defn check [res] 
  (let [re (re-pattern (str/join "|" res))]
    (and (every? #(re-find re %) winners)
         (not (some #(re-find re %) losers)))))

(defn get-rand-gt1 [res]
  (let [cands (filter #(> (count %) 1) res)]
    (if (seq cands) (rand-nth cands) nil)))

; Randomly chop one regexp into two with |
(defn chop [res]
  (let [cand (get-rand-gt1 res)]
    (if cand
      (sets/union (disj res cand)
                  (set (map str/join (split-at (inc (rand-int (dec (count cand)))) cand )))))))

; Randomly replace a character with a dot
(defn dot [res]
  (let [cand (get-rand-gt1 res)]
    (when cand
      (conj (disj res cand)
            (str/join (assoc (vec cand) (rand-int (count cand)) ".")) ))))


; Randomly remove an entire regexp
(defn yank [res] (disj res (rand-nth (seq res))))

; Randomly add back a winner
(defn add-back [res]
  (conj res (str "^" (rand-nth (seq  winners)) "$")))

; Randomly remove first or last (equivalent to some combination of chop and yank)
(defn decapitate [res]
  (let [cand (get-rand-gt1 res)]
    (when cand
      (conj (disj res cand) (str/join ((nth [next drop-last] (rand-int 2)) cand))))))

; Define perturbations by relative probability
(def perturbations
  (reduce #(conj %1 [(+ (first (last  %1)) (first %2)) (second %2)])
          [[0.0 identity]]
          [[100 add-back]
           [500 yank]
           [100 dot]
           [100 decapitate]
           [500 chop]]))

; Apply a random perturbation
(defn perturb [res]
  (let [r (rand (first (last perturbations)))
        f (second (first (filter #(< r (first %)) perturbations)))]
    (f res)))

; Ooh, let's be all physicsy.  Just adding up length of the regexp.
(defn energy [res] (+ (count res) (reduce + (map count res))))

; An iterative step of MH
;  res  - set of |-able regexp components
;  E    - its energy
;  T    - current temperature
;  dT   - will multiply T by (1-dT) at each step
(defn step [[res E T dT n dn]]
  (let [res2     (perturb res)
        E2       (energy res2)
        [res E]  (if (and (check res2) (> (Math/exp (/  (- E E2) T)) (rand))) [res2 E2] [res E])
        ret      [res E (* T (- 1.0 dT)) dT (inc n) dn]
        rx       (str/join "|" res)]
    (when (zero? (mod n dn)) (println (count rx) rx T))
    ret
    ))
; (nth (iterate step [initial (energy initial) 100.0 0.00004]) 200000)
; (nth (iterate step [initial (energy initial) 5.0 0.0000002 0 100000]) 20000000)
;PF : n.e|ho|ls|a.t|a..i|j|^n|bu|v.l|ma|a.a|ay.|r.e$|li|po
;PN : a.a|a..i|j|li|a.t|i..n|bu|oo|n.e|ay.|r.e$|tr|ls|po|v.l
