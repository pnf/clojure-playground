(ns counts
  (:use clojure.data.zip.xml)
  (:require [clojure.string :as cs]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
))

; actually frequencies would work here
;(defn freq [freq0 coll] (reduce #(assoc %1 %2 (inc (%1 %2 0)))  freq0  coll))

;; (defn freq-tag1 [freq0 tag coll] 
;;   (reduce #(assoc-in %1 [tag %2] (or (and (%1 tag) 
;;                                           (inc ((%1 tag) %2 0)))
;;                                      1))
;;                   freq0
;;                   coll))


; todo: why do I need to filter out zero length lines?
; n.b. can't just append #(filter seq %) for some reason
(defn line->words [line] (filter seq (-> line
                                         cs/trim
                                         cs/lower-case
                                         (cs/split #"\W+"))))

; merge frequencies of collection with those at (freq0 tag)
;; (defn freq-tag [freq0 tag coll]
;;   (let [f  (frequencies coll)
;;         f0 (freq0 tag)]
;;     (assoc freq0 tag (if f0 (merge-with + f0 f) f))))
(defn freq-tag [freq0 tag coll]
    (merge-with (partial merge-with +) freq0 {tag (frequencies coll)}))

;{"shakespeare" {"the" 1500  "forsoth" 25} "dickinson" {"feathers" 2}}
; (lcount-tag freq0 "Author:" lines)
; Would like  this to work for "by William Shakespeare\n"
(defn lcount-tag [freq0 tag lines]
  (let [line (.trim (first lines))
        etc  (next lines)]
    (cond 
     (not (seq etc)) freq0
     (.startsWith line tag)
        (reduce #(freq-tag %1 line (line->words %2)) freq0  etc)
     :else (recur freq0 tag etc))))

(defn fcount-tag [freq0 tag fname]
  (with-open [rdr (java.io.BufferedReader. 
                   (java.io.FileReader. fname))]
    (lcount-tag freq0 tag (line-seq rdr))
))

(def files (for [f (file-seq (clojure.java.io/file "/Users/pnf/dev/learn-clojure/tmp"))
                 :when (.isFile f)] (.getPath f)))

;(def mondo (reduce #(fcount-tag %1 "Author:" %2) {} files))
(def dogpile (reduce (partial merge-with +) (vals mondo)))
(def mondo100 (reduce #(fcount-tag %1 "Author:" %2) {} (take 100 files)))
(def dogpile100 (reduce (partial merge-with +) (vals mondo)))

(defn fdiv [a b] (float (/ a b)))

(defn normalize-map [m]
  (let [sum (reduce + (vals m))]
    (into {} (for [[t v] m] [t (fdiv v sum)]))))

; prior probability for author is weighted by total words in his corpus
(defn tag-priors [freqs]
  (let [sums  (for [[_ m] freqs] (reduce + (vals m)) )  ; vector total observed
        denom (reduce + sums)]
    (into {} (map #(vector %1 (float (/ %2 denom))) (keys freqs) sums))))

; prob for N=0 by tag.  Normalized by estimated number of words in language, but not by words in tag corpus
(defn tag-zeros [freqs]
  (let [ones  (for [[_ m] freqs] (count (filter #(= 1 %) (vals m))))]  ;vector of one counts
    ;(println ones)
    (into {} (map #(vector %1 (* 1e-6 %2)) (keys freqs) ones))))

(defn tag-sums [freqs]
  (into {}  (for [[t m] freqs] [t (reduce + (vals m))])))

; topic weights for a text, 
(defn tag-weights [freqs words]
  (let [tp (tag-priors freqs)
        tz (tag-zeros freqs)
        tn (tag-sums freqs)]
    ;(println tp tz tn)
    (normalize-map 
     (into {} (for [[t m] freqs]
               [t  (* (tp t)
                      (reduce * (map #(fdiv (or (m %) (tz t)) (tn t))
                                     words)))
                ]
               )))))

; with counts modified by top weight
(defn weighted-freq [freqs weights sums word]
    (reduce + (for [[t w] weights] (* w (fdiv (or ((freqs t) word) 0.0) (sums t)   )  ))))

;; (defn split-word [word]
;;   (->> (range (+ 1 (coint word)))
;;        (map #(split-at % word))
       
;; )

(defn edits [word]
  (let
      [abc        (seq "abcdefghijklmnopqrstuvwxyz")
       ;splits     (map #(list (apply str (take %1 word))
       ;                       (apply str (drop %1 word))) (-> word count inc range))
       splits    (map #(list (subs word 0 %) (subs word %)) (range (inc (count word))))
       deletes    (for [ [p1 p2] splits :when (seq p2) ] (str p1 (subs p2 1)))
       transposes (for [ [p1 p2] splits :when (> (count p2) 1)] (str p1 (subs p2 1 2) (subs p2 0 1) (subs p2 2)))
       replaces   (for [ [p1 p2] splits :when (seq p2), l abc   ] (str p1 l (subs p2 1)))
       inserts    (for [ [p1 p2] splits,  l abc] (str p1 l p2))
       ]
    (set (concat deletes transposes replaces inserts))
    ))

; (defn li [i] (cons i (lazy-seq (do (println "doing" i) (li (inc i))))))
; (seq (filter #(= 3 %) (take 5 (li 0))))  ; only does 3
; (seq (filter #(= 6 %) (take 5 (li 0))))  ; does 5, returs nil
; (some #(= 5 %) (li 3))  
; (map count (take 3 (editn ["boffo"]))) --> (1 284 88436)
; (map #(count (filter dogpile %)) (take 3 (editn ["boffo"]))) --> (0 0 32)
; (take 3 (map #(count (filter dogpile %)) (editn ["boffo"])))  ; map is lazy!
; (count (some seq (take 3 (map #(filter dogpile %) (editn ["boffo"])))))
; will return [words]  (mapcat edits words)  (mapcat (mapcat edits words)) etc. lazily
; iterate 
;(defn editn [words] (cons words (lazy-seq (editn (mapcat edits words)))))
(defn editn [words] (iterate (partial mapcat edits) words))

(defn correct [freq word]
  (apply max-key
         #(freq % 0)
         (or (some seq (take 3 (map #(filter freq %) (editn [word]))))
             [word])))

(defn correct-sentence [freq line] (map (partial correct freq) (line->words line)))

(defn correct2 [freqs freq weights sums word]
  (apply max-key 
         #(or (weighted-freq freqs weights sums %) 0)
         (or (some seq (take 3 (map #(filter freq %) (editn [word]))))
             [word])))

(defn correct-sentence2 [freqs line]
  (let [words   (line->words line)
        freq    (reduce (partial merge-with +) (vals freqs))
        weights (tag-weights freqs words)
        sums    (tag-sums freqs)]
    ;(println (cs/join ";" (keys freqs)) "\n" (vals weights) "\n" (vals sums))
    (map (partial correct2 freqs freq weights sums) (line->words line))))

(defn tag-guess [freqs line]
  (let [words (line->words line)
       weights (tag-weights freqs words)]
    (println (cs/join "\n" weights))))
