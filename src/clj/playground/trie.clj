(ns playground.trie)


;; (def mytrie (-> {} (put "joe" 3) (put "phil" 4) (put "phillip" 5))) mytrie
;;    ==> {\p {\h {\i {\l {\l {\i {\p {:value 5}}}, :value 4}}}}, \j {\o {\e {:value 3}}}}
(defn put [trie key val]
  (assoc-in trie (-> key seq vec (conj :value)) val))

;; (modify mytrie inc)
;;    ==> {\p {\h {\i {\l {\l {\i {\p {:value 6}}}, :value 5}}}}, \j {\o {\e {:value 4}}}}
(defn modify [trie f]
  (reduce (fn [m [k v]]  (assoc m k (if (= :value k) (f v) (modify v f))))
          trie trie))

(defn del [trie key]
  (let [key   (-> "joe" seq vec)]
    (assoc-in trie key (dissoc (get-in trie key) :value))))

