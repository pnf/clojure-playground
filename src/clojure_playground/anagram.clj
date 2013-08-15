(ns anagram
  (:require [clojure.java.io :as io])
  (:import [java.util Arrays ArrayList HashMap]
           [java.nio.file FileSystems Files]
           [java.nio.charset Charset]))
 
;; Everything other than longest-fast-function was stolen from David Nolan.
;; =============================================================================
;; Paolo Style Fast Version 0.06 secs on MBA 1.7ghz
 
(defn ^ArrayList words-fast []
  (let [path (.getPath (FileSystems/getDefault) "resources" (into-array String ["sowpods.txt"]))]
    (Files/readAllLines path (Charset/forName "US-ASCII"))))
 
(defn longest-fast []
  (let [ws   (words-fast)
        seen (HashMap.)]
    (loop [i 0 maxlen 0 result nil]
      (if (< i (.size ws))
        (let [^String word (.get ws i)
              arr  (.toCharArray word)
              _    (Arrays/sort arr)
              h    (String. arr)
              wlen (.length h)]
          (if (> wlen maxlen)
            (if-let [word' (.get seen h)]
              (recur (inc i) wlen [word' word])
              (do
                (.put seen h word)
                (recur (inc i) maxlen result)))
            (recur (inc i) maxlen result)))
        result))))

(defn longest-fast-functional []
  (let [ws   (words-fast)]
    (loop [i 0 maxlen 0 result nil seen (hash-map)]
      (if (< i (.size ws))
        (let [^String word (.get ws i)
              arr  (.toCharArray word)
              _    (Arrays/sort arr)
              h    (String. arr)
              wlen (.length h)]
          (if (> wlen maxlen)
            (if-let [word' (seen h) #_(.get seen h)]
              (recur (inc i) wlen [word' word] (hash-map))
              (recur (inc i) maxlen result (assoc seen h word)))
            (recur (inc i) maxlen result seen)))
        result))))

 
(comment
  (time (longest-fast))
  )
 
;; =============================================================================
;; Darius Style Idiomatic Version, 0.95 secs
 
(defn longest-short []
  (let [words (line-seq (io/reader "resources/sowpods.txt"))
        pals  (reduce (fn [m w]
                        (update-in m [(apply str (sort w))]
                          (fnil conj []) w))
                {} words)]
    (get pals
      (apply max-key count
        (for [[k v] pals
              :when (< 1 (count v))]
          k)))))
 
(comment
  (time (longest-short))
  )
