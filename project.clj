(defproject playground "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots"
                               :update :always}]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.xml "0.0.7"]
                 [org.clojure/data.zip "0.1.1"]
                 [cascalog/cascalog-core "2.0.0-SNAPSHOT"]
                 [org.apache.hadoop/hadoop-core "1.0.3"]
                 [org.clojure/core.async "0.1.0-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/core.typed "0.2.19"]
                 [org.apache.commons/commons-email "1.3.1"]
                 [org.zeromq/cljzmq "0.1.2-SNAPSHOT"]
                 [org.clojure/clojurescript "0.0-1853"]
                 [org.clojure/core.match "0.2.0"]
                 [com.cemerick/piggieback "0.1.0"]
                 [compojure "1.1.5"]
                 [jayq "2.4.0"]
                 [ring "1.2.0"]
                 [prismatic/dommy "0.1.1"]
                 [org.clojure/algo.monads "0.1.4"]
                 [com.datomic/datomic-free "0.8.4159"]]


  :plugins [[lein-cljsbuild "0.3.2"]
            [lein-ring "0.8.5"]]

  :ring {:handler playground.ring/handle}

  ; So ordinary clj repl can be turned into a clojurescript repl via (browser-repl)
  :injections [(require '[cljs.repl.browser :as brepl]
                        '[cemerick.piggieback :as pb])
               (defn browser-repl []
                 (pb/cljs-repl :repl-env
                               (doto (brepl/repl-env :port 9000)
                                      cljs.repl/-setup)))]

  :source-paths ["src/clj"]

  :cljsbuild 
  {; :crossovers [nanfott.parse]
   :builds
   [{:id "playground"
     :source-paths ["src/cljs"]
     :compiler {:optimizations :whitespace
                :pretty-print true
                :output-dir "out" 
                :output-to "resources/public/js/main.js"
                }}]}


  :jvm-opts ["-Djava.library.path=/usr/lib:/usr/local/lib -Xmx768m"]
  :java-source-paths ["src/java"]
)







