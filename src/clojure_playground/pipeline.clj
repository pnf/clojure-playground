(ns clojure.playground-pipeline
  (:require [ clojure.core.async :as async :refer [<!! >!! >! <! chan go]]))


(defn enter-pipeline [fns x link out]
  (go (loop [arg       x
             [f & fs]  fns]
        (go (try (let [res (f arg)]
                   (>! link res))
                 (catch Exception e (do (async/close! link)
                                        (>! out e)))))
        (if (seq fs)
          (recur (<! link) fs)
          (do (>! out (<! link))
              (async/close! link))))))

(defn pipeline [& fns]
  (let [in    (chan)
        out   (chan)]
    (go (loop [] (let [x    (<! in)
                       link (chan)]
                   (if (nil? x) 
                     (do 
                       (async/close! out) (async/close! link))
                     (do (enter-pipeline fns x link out)
                         (recur))))))
      [in out]))

(let [[in out] (pipeline
                #(/ 1 %)
                #(str "inverse=" %))]
  (>!! in 5) (assert (= "inverse=1/5" (<!! out)))
  (>!! in 0) (assert (instance? Exception (<!! out))))
