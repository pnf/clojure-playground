;;;;;;;;;;;;;;;

(def idx (clojure.data.xml/parse (java.io.FileReader. "/Users/pnf/dev/learn-clojure/index.xml")))
(def idz (clojure.zip/xml-zip idx))
(def ids (xml-> idz :etext (attr :rdf/ID)))
(def authors (xml-> idz :etext :creator text))
(def langs  (xml-> idz :etext :language text))
; http://www.gutenberg.org/files/15418/15418-8.txthttp://www.gutenberg.org/files/15418/15418-8.txt
(defrecord Book [id author lang])
(def books (map (fn [id author lang] (Book. id author lang)) ids authors langs))

(defn fetch-url[address]
  (with-open [stream (.openStream (java.net.URL. address))]
    (let  [buf (java.io.BufferedReader. 
                (java.io.InputStreamReader. stream))]
      (println "Fetching " address)
      (apply str (line-seq buf)))))

(defn fetch-try [urls]
  (if-let [u (first urls)]
    (try (fetch-url u)
         (catch Exception e (fetch-try (rest urls))))
    ""))

(defn kill-before [txt marker n]
  (let [i (.indexOf txt marker)]
    (if (>= i 0) (subs txt (+ n i)) txt)))
(defn kill-after [txt marker n]
  (let [i (.indexOf txt marker)]
    (if (>= i 0) (subs txt 0 (- i n)) txt)))

(defn clean [txt]
  (-> txt
      (kill-before "*** START OF THIS PROJECT" 25)
      (kill-after "*** END OF THIS PROJECT" 0)))

(defn fetch-id [id]
  (let [num (subs id 5)
        txt (fetch-try [(str "http://www.gutenberg.org/files/" num "/" num "-8.txt")
                        (str "http://www.gutenberg.org/files/" num "/" num "-0.txt")
                        (str "http://www.gutenberg.org/files/" num "/" num ".txt")])]
    (clean txt)
))

(defn append-id [fn,id]
  (with-open [out (io/writer fn :append true)]
    (.write out (fetch-id id))
))

(defn write-langs [books]
  (reduce (fn [files,book] (let [fname (str "words." (:lang book))]
                             (append-id fname (:id book))
                             (println "Appending to " fname)
                             (conj files fname)))
          #{}  books))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
