(ns clojurescript-playground.async
  (:require [cljs.core.async :refer [>! <! chan put! take! timeout close! dropping-buffer]]
            [cljs.core.match]
            [goog.dom :as dom]
            ;[clojure.browser.dom :as dom]
            [dommy.utils :as utils]
            [dommy.core :as dmy]
            [dommy.attrs :as attrs]
            [clojure.string :as str]
            [clojure.browser.repl :as repl]
            [jayq.core :as jq]
            )
  (:use [jayq.core :only [$ css html]])
  (:use-macros
   [dommy.macros :only [sel sel1 node]]
   [cljs.core.match.macros :only [match]]
   )
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
)


(defn ^:export go-cljs []
  (let [first (chan)
        last (loop [i 0 last first]
               (if (< i 100000)
                 (let [next (chan)]
                   (take! last (fn [v] (put! next (inc v))))
                   (recur (inc i) next))
                 last))]
    #_(go (let [s  (js/Date.)
              el (.getElementById js/document "cljs-time")]
          (>! first 0)
          (set! (.-innerHTML el)
            (str (<! last) " elapsed ms: " (- (js/Date.) s)))))))
