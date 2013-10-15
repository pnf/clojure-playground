(ns playground.async
  (:refer-clojure :exclude [map filter distinct remove])
  (:require [cljs.core.async :refer [>! <! chan put! take! timeout close!]]
            [goog.dom :as dom])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

#_(ns playground.async
  (:require [cljs.core.async :refer [>! <! chan put! take! timeout close! dropping-buffer]]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :refer [go alt!] :as async])
)

(def foo (chan))
(.log js/console "Loading1")
(go (.log js/console "Loading2"))

(defn start []
  (.log js/console "Dummy start method")
)


;^:export 
(defn go-cljs []
  (let [first (chan)
        last (loop [i 0 last first]
               (if (< i 10)
                 (let [next (chan)]
                   (.log js/console (str "Creating channel" i) )
                   (take! last (fn [v] (put! next (inc v))))
                   (recur (inc i) next))
                 last))]
    (go (let [s  (js/Date.)
              el (.getElementById js/document "cljs-time")]
          (.log js/console "Sending first message")
          (>! first 0)
          (.log js/console "waiting...")
          (set! (.-innerHTML el)
                (str (<! last) " elapsed ms: " (- (js/Date.) s)))))))
