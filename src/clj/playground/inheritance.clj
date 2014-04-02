(ns playground.inheritance)

(defn dfs [o f-pre f-seen f-kids]
  (loop [ [[o depth] & stack]  (list [o 0])
          seen                #{}         ]
    (when o
      (f-pre depth o)
      (if (seen o)
        (do 
          (f-seen depth)
          (recur stack seen))
        (recur (concat stack (map vector (f-kids o) (repeat (inc depth))))
               (conj seen o))))))

(defn inh-graph [o]  (dfs (.getClass o)
                          #(println (apply str (repeat %1 " ")) %2)
                          #(println (apply str (repeat %1 " ")) "..." )
                          #(let [ifcs (seq (.getInterfaces %))
                                 sc   (.getSuperclass %)]
                             (if sc (conj ifcs sc) ifcs))))
