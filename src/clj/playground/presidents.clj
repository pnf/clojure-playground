(ns playground.presidents
  (:require [clojure.string :as str]
            [clojure.set :as sets])
  (:import (java.lang Math)))

(def winners
  "Anyone who ever won the presidency."
  (set (str/split "washington adams jefferson jefferson madison madison monroe monroe adams jackson jackson vanburen harrison polk taylor pierce buchanan lincoln lincoln grant grant hayes garfield cleveland harrison cleveland mckinley mckinley roosevelt taft wilson wilson harding coolidge hoover roosevelt roosevelt roosevelt roosevelt truman eisenhower eisenhower kennedy johnson nixon nixon carter reagan reagan bush clinton clinton bush bush obama obama" #" ")))

(def losers 
  "Anyone who ran as a major party candidate but never won"
  (sets/difference  (set (str/split "clinton jefferson adams pinckney pinckney clinton king adams jackson adams clay vanburen vanburen clay cass scott fremont breckinridge mcclellan seymour greeley tilden hancock blaine cleveland harrison bryan bryan parker bryan roosevelt hughes cox davis smith hoover landon wilkie dewey dewey stevenson stevenson nixon goldwater humphrey mcgovern ford carter mondale dukakis bush dole gore kerry mccain romney" #" ")) winners ))




(def initial
  "Set of all winners, each sandwiched with anchors."
  (set (map #(str "^" % "$") winners)))

(defn check
  "Verify that this set of regexps, or'd, will match winners but not losers"
  [res] 
  (let [re (re-pattern (str/join "|" res))]
    (and (every? #(re-find re %) winners)
         (not (some #(re-find re %) losers)))))

(defn get-rand-gt1 
  "Fetch a re, as long as we have at least ne"
  [res]
  (let [cands (filter #(> (count %) 1) res)]
    (if (seq cands) (rand-nth cands) nil)))

(defn chop
  "Randomly chop one regexp into two with |"
  [res]
  (let [cand (get-rand-gt1 res)]
    (if cand
      (sets/union (disj res cand)
                  (set (map str/join (split-at (inc (rand-int (dec (count cand)))) cand )))))))

; 
(defn dot
  "Randomly replace a character with a dot"
  [res]
  (let [cand (get-rand-gt1 res)]
    (when cand
      (conj (disj res cand)
            (str/join (assoc (vec cand) (rand-int (count cand)) ".")) ))))


; 
(defn yank
  "Randomly remove an entire regexp"
  [res] (disj res (rand-nth (seq res))))
; 
(defn add-back
  "Randomly add back a winner"
  [res]
  (conj res (str "^" (rand-nth (seq  winners)) "$")))


(defn decapitate
  "Randomly remove first or last (equivalent to some combination of chop and yank)"
  [res]
  (let [cand (get-rand-gt1 res)]
    (when cand
      (conj (disj res cand) (str/join ((nth [next drop-last] (rand-int 2)) cand))))))

(def perturbations
  (reduce #(conj %1 [(+ (first (last  %1)) (first %2)) (second %2)])
          [[0.0 identity]]
          ;; Perturbations with relative probability
          [[100 add-back]
           [500 yank]
           [100 dot]
           [100 decapitate]
           [100 chop]]))

; Apply a random perturbation
(defn perturb
  "Apply one of our perturbations randomly"
  [res]
  (let [r (rand (first (last perturbations)))
        f (second (first (filter #(< r (first %)) perturbations)))]
    (f res)))

(defn energy 
  "Our energy function is just the length of the regexp"
  [res]
  (when (check res) (dec  (+ (count res) (reduce + (map count res))))))

(defn steps
  "An iterative step of MH
     state0 - initial state
     energy - fn that calculates energy of state
     T    - current temperature
     dT   - will multiply T by (1-dT) at each step"
  [state0 energy T dT]
  (letfn [(step [[state E T]]
            (let [state2     (perturb state)
                  E2       (energy state2)
                  [state E]  (if (and E2 (> (Math/exp (/  (- E E2) T)) (rand)))
                               [state2 E2] [state E])]
              [state E (* T (- 1.0 dT))]))]
    (iterate step [state0 (energy state0) T])))

(defn annotate
  "Filter output of MH step sequence, annotating interesting ones, either every dnth or when
   energy is reduced by dE.  If either is nil, don't comment on that."
  [steps dn dE]
  (letfn [(annotate* [steps n minE]
            (let [step           (first steps)
                  [state E T] step
                  newMinE     (if minE (min E minE) E)
                  out1        (when (and dE minE (<= dE (- minE newMinE))) (str "*** " E " " (str/join "|" (first step))))
                  out2        (when (and dn (zero? (mod n dn))) (str n " " step))
                  tail        (lazy-seq (annotate* (next steps) (inc n) newMinE))]
              (if (or out1 out2) (cons  (str out1 out2) tail) tail)))]
    (annotate* steps 0 nil)))

(defn print-all
  "Print everything from a sequence as fast as possible, discarding head."
  [xs] (doseq [x  xs] (println x)))

;(print-all (take 10000 (annotate (steps initial energy 5.0 0.0000001) nil 1)))
;PF : n.e|ho|ls|a.t|a..i|j|^n|bu|v.l|ma|a.a|ay.|r.e$|li|po
;PN : a.a|a..i|j|li|a.t|i..n|bu|oo|n.e|ay.|r.e$|tr|ls|po|v.l
