(ns nanfott.repl
 (:require [clojure.browser.repl :as repl]))
(.log js/console  "Attempting to connect on 9000")
(def ret (repl/connect "http://localhost:9000/repl"))
