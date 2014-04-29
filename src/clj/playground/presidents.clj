(ns playground.presidents
  (:require [clojure.string :as str]
            [clojure.set :as sets])
  (:import (java.lang Math)))

#_(def winners
  "Anyone who ever won the presidency."
  (set (str/split "washington adams jefferson jefferson madison madison monroe monroe adams jackson jackson vanburen harrison polk taylor pierce buchanan lincoln lincoln grant grant hayes garfield cleveland harrison cleveland mckinley mckinley roosevelt taft wilson wilson harding coolidge hoover roosevelt roosevelt roosevelt roosevelt truman eisenhower eisenhower kennedy johnson nixon nixon carter reagan reagan bush clinton clinton bush bush obama obama" #" ")))

#_(def winners (set (str/split  "acritan aesthophysiology amphimictical baruria calomorphic disarmature effusive fluted fusoid goblinize nihilistic noisefully picrorhiza postarytenoid revolutionize suprasphanoidal suspenseful tapachula transmit unversatile vibetoite" #" " )))

(def winners (set (str/split  "000000000 000000003 000000006 000000009 000000012 000000015 066990060 140091876 173655750 312440187 321769005 368542278 390259104 402223947 443512431 714541758 747289572 819148602 878531775 905586303 953734824" #" " )))

#(def losers 
  "Anyone who ran as a major party candidate but never won"
  (sets/difference  (set (str/split "clinton jefferson adams pinckney pinckney clinton king adams jackson adams clay vanburen vanburen clay cass scott fremont breckinridge mcclellan seymour greeley tilden hancock blaine cleveland harrison bryan bryan parker bryan roosevelt hughes cox davis smith hoover landon wilkie dewey dewey stevenson stevenson nixon goldwater humphrey mcgovern ford carter mondale dukakis bush dole gore kerry mccain romney" #" ")) winners ))

(def losers 
  (sets/difference  (set (str/split "abba anallagmatic bassarisk chorioallantois coccomyces commotive engrammatic glossoscopia hexacoralla hippogriffin inflammableness otto overattached saffarid sarraceniaceae scillipicrin tlapallan trillion unclassably unfitting unsmelled warrandice" #" ")) winners ))

(def losers 
  (sets/difference  (set (str/split " 000000005 000000008 000000010 000000011 000000014 018990130 112057285 159747125 176950268 259108903 333162608 388401457 477848777 478621693 531683939 704168662 759282218 769340942 851936815 973816159 979204403" #" ")) winners ))
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
  "Fetch a re, of length at least two as long as we have at least one"
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
     S0 - initial state
     energy - fn that calculates energy of state
     perturb - fn to perturb the state
     T    - current temperature
     dT   - will multiply T by (1-dT) at each step"
  [S0 energy perturb T dT]
  (letfn [(step [[S E T]]
            (let [S2     (perturb S)
                  E2       (energy S2)
                  [S E]  (if (and E2 (> (Math/exp (/  (- E E2) T)) (rand)))
                               [S2 E2] [S E])]
              [S E (* T (- 1.0 dT))]))]
    (iterate step [S0 (energy S0) T])))

(defn annotate
  "Filter output of MH step sequence, annotating interesting ones, either every dnth or when
   energy is reduced by dE.  If either is nil, don't comment on that."
  [steps dn dE]
  (letfn [(annotate* [steps n minE]
            (let [[S E T]           (first steps)
                  newMinE     (if minE (min E minE) E)
                  out1        (when (and dE minE (<= dE (- minE newMinE))) (str "*** " E " " (str/join "|" S)))
                  step        (first steps)
                  out2        (when (and dn (zero? (mod n dn))) (str n " " step))
                  tail        (lazy-seq (annotate* (next steps) (inc n) newMinE))]
              (if (or out1 out2) (cons  (str out1 out2) tail) tail)))]
    (annotate* steps 0 nil)))

(defn print-all
  "Print everything from a sequence as fast as possible, discarding head."
  [xs] (doseq [x  xs] (println x)))

;(print-all (take 10000 (annotate (steps initial energy perturb 5.0 0.0000001) nil 1)))
;PF : n.e|ho|ls|a.t|a..i|j|^n|bu|v.l|ma|a.a|ay.|r.e$|li|po
;PN : a.a|a..i|j|li|a.t|i..n|bu|oo|n.e|ay.|r.e$|tr|ls|po|v.l
